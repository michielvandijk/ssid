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

# Nat urban-rural projections
nat_urban_rural_proj <- readRDS(file.path(param$model_path,
                                             glue("benchmark/nat_urban_rural_proj_{param$iso3c}.rds")))
nat_urban_rural_proj_m15 <- readRDS(file.path(param$model_path,
                                                 glue("benchmark/nat_urban_rural_proj_m15_{param$iso3c}.rds")))

# Subnat age_sex projections
nat_age_sex_proj <- readRDS(file.path(param$model_path,
                                         glue("benchmark/nat_age_sex_proj_{param$iso3c}.rds")))
nat_age_sex_proj_m15 <- readRDS(file.path(param$model_path,
                                             glue("benchmark/nat_age_sex_proj_m15_{param$iso3c}.rds")))

# Subnat occupation projections
nat_occ_proj <- readRDS(file.path(param$model_path,
                                     glue("benchmark/nat_occupation_proj_{param$iso3c}.rds")))
nat_occ_proj_m15 <- readRDS(file.path(param$model_path,
                                         glue("benchmark/nat_occupation_proj_m15_{param$iso3c}.rds")))

# Subnat household projections
nat_hh_proj <- readRDS(file.path(param$model_path,
                                    glue("benchmark/nat_hh_proj_{param$iso3c}.rds")))
nat_hh_proj_m15 <- readRDS(file.path(param$model_path,
                                        glue("benchmark/nat_hh_proj_m15_{param$iso3c}.rds")))


# ========================================================================================
# PREPARE -------------------------------------------------------------------------------
# ========================================================================================

# Create region - target zone id
adm_list <- adm_list %>%
  dplyr::select(target_zone = adm0_code, region = adm0_code) %>%
  distinct() %>%
  mutate(reg_tz = paste(region, target_zone, sep = "-x-"))

# check totals
nat_age_sex_proj %>%
  group_by(scenario, year) %>%
  summarize(value = sum(value),
            .groups = "drop")
nat_hh_proj


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


# BENCHMARK --------------------------------------------------------------------------------
prepare_benchmark_nat <- function(hh, u_r, a_s, oc){

  # Extract hh_number for all zones
  hh_number <- hh %>%
    dplyr::select(target_zone = adm0_code, region = adm0_code, scenario, year, n = value) %>%
    distinct()

  # Extract per targets for all zones
  per_urban_rural <- u_r %>%
    dplyr::select(target_zone = adm0_code, region = adm0_code, urban_rural, scenario, year, value) %>%
    distinct() %>%
    pivot_wider(names_from = urban_rural, values_from = value, values_fill = 0)

  per_age <- a_s %>%
    dplyr::select(target_zone = adm0_code, region = adm0_code, scenario, year, age, value) %>%
    group_by(scenario, year, target_zone, region, age) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = age, values_from = value, values_fill = 0)

  per_sex <- a_s %>%
    dplyr::select(target_zone = adm0_code, region = adm0_code, scenario, year, sex, value) %>%
    group_by(scenario, year, target_zone, region, sex) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = sex, values_from = value, values_fill = 0)

  per_occ <- oc %>%
    dplyr::select(target_zone = adm0_code, region = adm0_code, scenario, year, occ, value) %>%
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

bm_by <- prepare_benchmark_nat(nat_hh_proj, nat_urban_rural_proj, nat_age_sex_proj, nat_occ_proj)


# ========================================================================================
# RUN NATIONAL REGION, PER YEAR, PER SSP -------------------------------------------------
# ========================================================================================

temp_path <- file.path(param$model_path, glue("simulation/{Sys.Date()}/national"))

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$base_year, stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))


tic()
library(ipfr)
# note that the output will be saved as ssp_y, overwriting spatial simulation results!
sim_by <- ssp_y$ssp_y[c(1)] %>%
  set_names() %>%
  map(reweigh, "ETH-x-ETH", hh_seed_by, per_seed_by, bm_by,
              verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
              absolute_diff = 10, parallel = TRUE, output = NULL)
toc()



# ========================================================================================
# CHECK CONVERGENCE ----------------------------------------------------------------------
# ========================================================================================

x <- sim_by[["ssp1_2018"]][["ETH-x-ETH"]]$weight_tbl
sim_by[["ssp1_2018"]][["ETH-x-ETH"]]$weight_dist
sim_by[["ssp1_2018"]][["ETH-x-ETH"]]$primary_comp
sim_by[["ssp1_2018"]][["ETH-x-ETH"]]$secondary_comp
sim_by[["ssp1_2018"]][["ETH-x-ETH"]]$stats
