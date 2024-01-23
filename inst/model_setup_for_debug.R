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
       janitor, furrr, progressr, terra, texreg, tidyverse, readxl, scales, sf, tictoc)

# R options
options(scipen = 999) # Suppress scientific notation
options(digits = 4)


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

# Set the folders where the scripts, model and database will be stored.
# Note that R uses forward slashes even in Windows!!

# Set ssid parameters
param <- ssid_par(
  model_path = "c:/Users/dijk158/OneDrive - Wageningen University & Research/Projects/ssid/ssid_bgd",
  db_path = "c:/Users/dijk158/OneDrive - Wageningen University & Research/data/microsim_db",
  iso3c = "BGD",
  seed_year = 2016,
  proj_year = 2020,
  start_year = 2016,
  end_year = 2050,
  adm_level = 2)


print(param)
