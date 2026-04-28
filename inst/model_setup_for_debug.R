# ========================================================================================
# Project:  ssid
# Subject:  Script to set model parameters
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
p_load(ssid, broom, countrycode, dotwhisker, here, ggspatial, glue, haven, imputeTS, ipumsr,
       janitor, furrr, progressr, terra, texreg, tidyverse, readxl, scales, sf, tictoc,
       laeken, ipfr, naniar)

# R options
options(scipen = 999) # Suppress scientific notation
options(digits = 4)


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

# Set the folders where the scripts, model and database will be stored.
# Note that R uses forward slashes even in Windows!!

# Creates a model folder structure in model_path. The user can replace the name
# with the country code of the case-study country or choose another new name.
# db_path sets the location of ssid_db.

# Set ssid parameters
param <- ssid_par(
  model_path = "J:/mids/mids_mex/mids",
  db_path = "J:/mids/mids_mex/data",
  iso3c = "MEX",
  micro_year = 2024,
  start_year = 2024,
  end_year = 2050,
  adm_level = 2)

print(param)

# set ssid_db
ssid_db_version <- "v0.0.1"

# set sim_version
#sim_version <- "2025-03-31"

# set magnet_version
#magnet_version <- "2025-03-27"
