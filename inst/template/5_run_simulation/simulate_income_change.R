# ========================================================================================
# Project:  simFNS
# Subject:  Script to simulate income change
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
} else {
  library(pacman)
}

# Load key packages
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue", "furrr", "progressr",
       "ipfr", "imputeTS", "haven")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Estimated income
est_income <- readRDS(file.path(proc_path, glue("simulation/estimated_income_{iso3c_sel}.rds")))

# hh_db
hh_db <- readRDS(file.path(proc_path, glue("simulation/hh_db_{iso3c_sel}.rds")))

# per_db
per_db <- readRDS(file.path(proc_path, glue("simulation/per_db_{iso3c_sel}.rds")))

# Spatial simulation
#version <- "2022-01-14"
version <- "2022-05-31"
sim_by <- readRDS(file.path(proc_path, glue("simulation/{version}/ssp1_3_by_{iso3c_sel}.rds")))
sim_no_by <- readRDS(file.path(proc_path, glue("simulation/{version}/ssp1_3_no_by_{iso3c_sel}.rds")))

# Adm_list
adm_list <- readRDS(file.path(proc_path, glue("adm/adm_list_{iso3c_sel}.rds")))

# Subnat population projections
subnat_age_sex_proj <- readRDS(file.path(proc_path,
                                         glue("benchmark/subnat_age_sex_proj_{iso3c_sel}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(proc_path,
                                             glue("benchmark/subnat_age_sex_proj_m15_{iso3c_sel}.rds")))

# MAGNET relative wage projections
wage_proj_raw <- readRDS(file.path(proc_path, glue("simulation/wage_proj_{iso3c_sel}.rds")))


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================

sim_db <- bind_rows(
  map_df(sim_no_by, ~map_df(.x, "weight_tbl", .id = "name"), .id = "name2")
  ) %>%
    separate(name, into = c("adm1_name", "adm2_code"), sep = "-x-") %>%
    separate(name2, into = c("scenario", "year"), sep = "_") %>%
  mutate(source = "simulation",
         year = as.integer(year))


# CHECK THIS CODE -------------------------------------------------------------------------
# We link the hh_db ONLY on hh_id because we are using the full survey in each adm2 simulation. 
# This means in the sim_db the same hh_id can occur in multiple adm1 and adm2.
# In the survey is bigger and we use the adm1 part of the survey as seed for all underlying 
# adm2, we can also join on adm1 (but this is perhaps not necessary)

# sim_db <- sim_db %>%
#   dplyr::select(adm1_name, adm2_code, hh_id = id, hh_weight = weight, source, year, scenario) %>%
#   left_join(hh_db)

sim_db <- sim_db %>%
  dplyr::select(adm1_name, adm2_code, hh_id = id, hh_weight = weight, source, year, scenario) %>%
  left_join(hh_db %>%
              select(hh_id, ea_id, urban_rural, survey_hh_weight, hh_size, hh_n_m15))

# select relevant years for wage proj
wage_proj <- wage_proj_raw %>%
  filter(year %in% c(2018, 2020, 2030, 2040, 2050))


# ========================================================================================
# CHECK SUM OF WEIGHTS BETWEEN SEED AND SIMULATION----------------------------------------
# ========================================================================================

# Note that the base year for the simulation is often different and more recent than the
# survey so differences are expected.

# Households
sum(hh_db$survey_hh_weight)
sim_db %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(hh_weight))

# Population
sum(hh_db$survey_hh_weight*hh_db$hh_size)
sim_db %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(hh_weight*hh_size))


# ========================================================================================
# PROJECT INCOME--------------------------------------------------------------------------
# ========================================================================================

# CALCULATE NUMBER OF CHILDREN -----------------------------------------------------------

# Determine actual m15 by adm2 in 2050 from projections
# Calculate projected number of children using 2018 number and 2050 weight
# Recalculate number of children by scaling original hh_n_m15.
# This implies number of children are not an integer anymore. This is not a problem as
# all hh have a weight so on average the number of children can be fractional, in reality
# the underlying distribution can be different integer number of children per hh.

# TO UPDATE. We also calculate scaling factors for the base year which should be 1 if ipfr
# would converge fully. For some adm2 this is not the case => check convergence statistics.

# m15 scaling factor for projections
m15_sf <- bind_rows(
  subnat_age_sex_proj %>%
    filter(scenario %in% unique(sim_db$scenario), year %in% unique(sim_db$year)) %>%
    filter(age == "m15") %>%
    mutate(source = "subnat_proj") %>%
    group_by(source, year, scenario, adm1_name, adm2_code) %>%
    summarize(m15 = sum(value, na.rm = TRUE), .groups = "drop"),
  sim_db %>%
    group_by(source, year, scenario, adm1_name, adm2_code) %>%
    summarize(m15 = sum(hh_weight*hh_n_m15), .groups = "drop")) %>%
  group_by(adm1_name, adm2_code, year, scenario) %>%
  summarize(sf = m15[source == "subnat_proj"]/m15[source == "simulation"],
            .groups = "drop")

sim_db <- sim_db %>%
  left_join(m15_sf) %>%
  mutate(hh_n_m15_s = hh_n_m15 * sf,
         hh_size_s = hh_size + hh_n_m15_s - hh_n_m15)


# SIMULATE HH INCOME ---------------------------------------------------------------------

# Calculate hh income by combining income projections and per income
# Filter out m15 as income is 0 by definition
sim_hh_income <- per_db %>%
  left_join(est_income$per_income) %>%
  filter(occupation != "m15") %>%
  left_join(wage_proj) %>%
  group_by(hh_id, year, scenario) %>%
  summarize(occ_hh_income = sum(per_income_est*occ_wage_proj),
            .groups = "drop")

# Update residual proportional to increase in income_occ if residual is not zero
sim_hh_income <- left_join(est_income$hh_income, sim_hh_income) %>%
  mutate(occ_hh_income_growth = ifelse(hh_income_ex_resid != 0, occ_hh_income/hh_income_ex_resid, 0),
         residual_update = occ_hh_income_growth*residual,
         hh_income_proj = occ_hh_income + residual_update,
         hh_income_growth = hh_income_proj/hh_income)


# SIMULATE PC_INCOME ----------------------------------------------------------------------

# Calculate per capita income by dividing proj hh income by household size
# Calculate person level weight by multiplying hh weight with household size.
sim_db <- sim_db %>%
  left_join(sim_hh_income) %>%
  mutate(per_income_proj = hh_income_proj/hh_size_s,
         per_weight = hh_weight*hh_size_s)

############## CHECK DIFFERENCE IN TOTALS. MUST BE IN HH_SIZE_S vs HH_SIZE
# CHECK how to treat not_in_lf. Now we estimate wage and add but perhaps set to zero contribution,
# simular to m15.
sim_db %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(hh_weight*hh_size),
            pop2 = sum(per_weight))

# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(sim_db, file.path(proc_path, glue("simulation/{version}/sim_db_{iso3c_sel}.rds")))
