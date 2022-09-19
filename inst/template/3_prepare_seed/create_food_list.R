# ========================================================================================
# Project:  simFNS
# Subject:  Calculate hh consumption
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
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue")

# Load additional packages
p_load("haven")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

param$iso3c <- "BGD"
survey_year <- 2016



# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================


# NOTES ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# Daily consumption
con1_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$iso3c}/hies_2016/data/HH_SEC_9A2.sav")))


con2_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$iso3c}/hies_2016/data/HH_SEC_9B2.sav")))



# ========================================================================================
# OBTAIN FOOD ITEM LIST
# ========================================================================================

# DAILY FOOD CONSUMPTION FOR 14 DAY PERIOD -----------------------------------------------

food_list_daily <- con1_raw %>%
  transmute(hh_id = hhold, day = day, item_code = as.integer(item), item = as_factor(item), quantity = s9a2q02,
            unit = as_factor(s9a2q03), value = s9a2q04, other = s9a2_os) %>%
  dplyr::select(item_code, item, unit) %>%
  distinct() %>%
  arrange(item_code) %>%
  zap_label() %>%
  na.omit()


# WEEKLY CONSUMPTION FOR 2 WEEK PERIOD ---------------------------------------------------

food_list_weekly <- con2_raw %>%
  transmute(hh_id = hhold, week = week, item_code = as.integer(s9bq01), item = as_factor(s9bq01), quantity = s9bq02,
            unit = as_factor(s9b2q03), value = s9bq04) %>%
  dplyr::select(item_code, item, unit) %>%
  distinct() %>%
  arrange(item_code) %>%
  zap_label() %>%
  na.omit()

# COMBINE --------------------------------------------------------------------------------

food_list <- bind_rows(food_list_daily, food_list_weekly)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

write_csv(food_list, file.path(param$model_path, "seed/food_item_list.csv"))

