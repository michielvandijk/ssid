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



# Michiel WECR
if(Sys.info()["user"] == "dijk158") {
  model_path <- "C:/Users/dijk158/OneDrive - Wageningen University & Research/Projects/2022_ssid_uga/ssid"
  db_path <-   "c:/Users/dijk158/OneDrive - Wageningen University & Research/data/microsim_db"
}

# Valerie WECR
if(Sys.info()["user"] == "janss208") {

}

# Set ssid parameters
param <- ssid_par(
  model_path = model_path,
  db_path = db_path,
  iso3c = "UGA",
  base_year = 2018,
  start_year = 2018,
  end_year = 2050,
  adm_level = 2)

print(param)
