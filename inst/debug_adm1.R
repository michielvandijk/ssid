# ========================================================================================
# Project:  ssid
# Subject:  Script to create adm maps for analysis
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load required packages
p_load(ssid, broom, countrycode, dotwhisker, here, ggspatial, glue, haven, imputeTS, ipumsr, janitor, furrr, progressr,
       terra, tidyverse, readxl, scales, sf, texreg, tictoc)


# R options
options(scipen = 999) # Suppress scientific notation
options(digits = 4)


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================


# Set the folders where the scripts, model and database will be stored.
# Note that R uses forward slashes even in Windows!!

# Creates a model folder structure in c:/temp/ with the name 'ssid_eth'.
# the user can replace eth with the country code of the case-study country or
# choose a new name.
model_path <- "c:/Users/dijk158/OneDrive - Wageningen University & Research/Projects/ssid/ssid_eth"

# Set location of ssid_db
db_path <-   "c:/Users/dijk158/OneDrive - Wageningen University & Research/data/microsim_db"

# Set ssid parameters
param <- ssid_par(
  model_path = model_path,
  db_path = db_path,
  iso3c = "ETH",
  base_year = 2018,
  start_year = 2018,
  end_year = 2050,
  adm_level = 2)

print(param)


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Seed
hh_db <- readRDS(file.path(param$model_path, glue("seed/hh_db_{param$iso3c}.rds")))
per_db <- readRDS(file.path(param$model_path, glue("seed/per_db_{param$iso3c}.rds")))

# Adm_list
adm_list <- readRDS(file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))


# Subnat urban-rural projections
subnat_urban_rural_proj <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_urban_rural_proj_{param$iso3c}.rds")))
subnat_urban_rural_proj_m15 <- readRDS(file.path(param$model_path,
                                                 glue("benchmark/subnat_urban_rural_proj_m15_{param$iso3c}.rds")))

# Subnat age_sex projections
subnat_age_sex_proj <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_age_sex_proj_{param$iso3c}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_age_sex_proj_m15_{param$iso3c}.rds")))

# Subnat occupation projections
subnat_occ_proj <- readRDS(file.path(param$model_path,
                                     glue("benchmark/subnat_occupation_proj_{param$iso3c}.rds")))
subnat_occ_proj_m15 <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_occupation_proj_m15_{param$iso3c}.rds")))

# Subnat household projections
subnat_hh_proj <- readRDS(file.path(param$model_path,
                                    glue("benchmark/subnat_hh_proj_{param$iso3c}.rds")))
subnat_hh_proj_m15 <- readRDS(file.path(param$model_path,
                                        glue("benchmark/subnat_hh_proj_m15_{param$iso3c}.rds")))

# ========================================================================================
# PREPARE -------------------------------------------------------------------------------
# ========================================================================================

# Create adm1 benchmark data
# adm1 urban-rural projections
adm1_urban_rural_proj <- subnat_urban_rural_proj %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario, urban_rural) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

adm1_urban_rural_proj_m15 <- subnat_urban_rural_proj_m15 %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario, urban_rural) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

# adm1 age_sex projections
adm1_age_sex_proj <- subnat_age_sex_proj %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario, age, sex) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

adm1_age_sex_proj_m15 <- subnat_age_sex_proj_m15 %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario, age, sex) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

# adm1 occupation projections
adm1_occ_proj <- subnat_occ_proj %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario, occ) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

adm1_occ_proj_m15 <- subnat_occ_proj_m15 %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario, occ) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

# adm1 household projections
adm1_hh_proj <- subnat_hh_proj %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)

adm1_hh_proj_m15 <- subnat_hh_proj_m15 %>%
  group_by(adm0_code, adm1_name, adm1_code, year, scenario) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(adm2_name = adm1_name, adm2_code = adm1_code)


# Create region - target zone id
adm_list <- adm_list %>%
  dplyr::select(target_zone = adm1_code, region = adm1_name) %>%
  distinct() %>%
  mutate(reg_tz = paste(region, target_zone, sep = "-x-"))

# check totals
adm1_age_sex_proj %>%
  group_by(scenario, year) %>%
  summarize(value = sum(value),
            .groups = "drop")
adm1_hh_proj


# ========================================================================================
# BASE YEAR SIMULATION -------------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# For the base year, we include the m15 category
# hh seed
hh_seed_by <- hh_db %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

per_seed_by <- per_db %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation)

oc <- adm1_occ_proj

# BENCHMARK --------------------------------------------------------------------------------
prepare_benchmark_adm1 <- function(hh, u_r, a_s, oc){

  # Extract hh_number for all zones
  hh_number <- hh %>%
    dplyr::select(target_zone = adm1_code, region = adm1_name, scenario, year, n = value) %>%
    distinct()

  # Extract per targets for all zones
  per_urban_rural <- u_r %>%
    dplyr::select(target_zone = adm1_code, region = adm1_name, urban_rural, scenario, year, value) %>%
    distinct() %>%
    pivot_wider(names_from = urban_rural, values_from = value, values_fill = 0)

  per_age <- a_s %>%
    dplyr::select(target_zone = adm1_code, region = adm1_name, scenario, year, age, value) %>%
    group_by(scenario, year, target_zone, region, age) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = age, values_from = value, values_fill = 0)

  per_sex <- a_s %>%
    dplyr::select(target_zone = adm1_code, region = adm1_name, scenario, year, sex, value) %>%
    group_by(scenario, year, target_zone, region, sex) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = sex, values_from = value, values_fill = 0)

  per_occ <- oc %>%
    dplyr::select(target_zone = adm1_code, region = adm1_name, scenario, year, occ, value) %>%
    group_by(scenario, year, target_zone, region, occ) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = occ, values_from = value, values_fill = 0)

  hh_bm <- list(hh_number = hh_number)
  per_bm <- list(
    per_urban_rural = per_urban_rural,
    per_age = per_age,
    per_sex = per_sex,
    per_occ = per_occ)
  bm <- list(hh_bm = hh_bm, per_bm = per_bm)
  return(bm)
}

bm_by <- prepare_benchmark_adm1(adm1_hh_proj, adm1_urban_rural_proj, adm1_age_sex_proj, adm1_occ_proj)

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$base_year, stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))


# SIMULATION FULL SAMPLE ------------------------------------------------------------------

temp_path <- file.path(param$model_path, glue("simulation/{Sys.Date()}/adm1"))
dir.create(temp_path, recursive = TRUE)

tic()
library(ipfr)
# note that the output will be saved as ssp_y, overwriting spatial simulation results!
sim_by <- ssp_y$ssp_y %>%
  set_names() %>%
  map(reweigh, adm_list$reg_tz, hh_seed_by, per_seed_by, param = param, bm_by, param = param,
              verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
              absolute_diff = 10, parallel = TRUE, output = temp_path)
toc()
str(sim_by)


# SIMULATION REGIONAL SAMPLE --------------------------------------------------------------

tic()
library(ipfr)
# note that the output will be saved as ssp_y, overwriting spatial simulation results!
sim_by <- ssp_y$ssp_y %>%
  set_names() %>%
  map(reweigh, adm_list$reg_tz, hh_seed_by, per_seed_by, bm_by, param = param,
      verbose = TRUE, reg_sample = TRUE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
      absolute_diff = 10, parallel = TRUE, output = temp_path)
toc()


# ========================================================================================
# NOT BY SIMULATION ----------------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# We exclude any families that only consist of m15 members. If not, ipu cannot solved as
# there is no matching data in per seed. In this case n = 1
hh_seed_no_by <- hh_db %>%
  filter(hh_size != hh_n_m15) %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

# We exclude the m15 category and distribute the number of children post process
per_seed_no_by <- per_db %>%
  rename(region = adm1_name) %>%
  filter(age != "m15") %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation) %>%
  droplevels()


# BENCHMARK -------------------------------------------------------------------------------

# We use the projections that exclude the m15 class
bm_no_by <- prepare_benchmark_adm1(adm1_hh_proj_m15, adm1_urban_rural_proj_m15,
                              adm1_age_sex_proj_m15, adm1_occ_proj_m15)


# SIMULATION FULL SAMPLE ------------------------------------------------------------------

# Takes # 87389.17 sec elapsed no parallel
ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"),
                     y = c(2020, 2030, 2040, 2050), stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
sim_no_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz, hh_seed_no_by, per_seed_no_by, bm_no_by, param = param,
                 verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
                 absolute_diff = 10, output = temp_path)
names(sim_no_by) <- ssp_y$ssp_y
toc()


# SIMULATION REGIONAL SAMPLE --------------------------------------------------------------


tic()
sim_no_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz, hh_seed_no_by, per_seed_no_by, bm_no_by, param = param,
                 verbose = TRUE, reg_sample = TRUE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
                 absolute_diff = 10, output = temp_path)
names(sim_no_by) <- ssp_y$ssp_y
toc()



# ========================================================================================
# CHECK CONVERGENCE ----------------------------------------------------------------------
# ========================================================================================

sim_db <- c(sim_by, sim_no_by)
sim_stats <- extract_output(sim_db, "stats")
table(sim_stats$stats_sum$converged)
table(sim_stats$stats_sum$worst_marginal_stats_category[!sim_stats$stats_sum$converged])

prim <- extract_output(sim_db, "primary_comp")
second <- extract_output(sim_db, "secondary_comp")

saveRDS(sim_db, file.path(temp_path, "sim_db_adm1.rds"))

# ========================================================================================
# PLOT WEIGHTS ---------------------------------------------------------------------------
# ========================================================================================

# UPDATE: Create function that compares across years per region?
# Create pdf to inspect weights of histogram
pdf(file = file.path(temp_path, "ssp1_3_weights.pdf")) # , width = 8.27, height = 11.69
extract_weight_plots(sim_db, adm_list$reg_tz, ssp_y$ssp_y)
dev.off()
