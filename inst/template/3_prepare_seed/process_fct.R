# ========================================================================================
# Project:  simfns_bgd
# Subject:  Script to clean food composition table
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue)

# Load additional packages
p_load(janitor)

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# LOAD -----------------------------------------------------------------------------------
# ========================================================================================

# food composition table
fct_raw <- read_excel(file.path(param$db_path, "food_composition_tables/BGD/FCDB_7_4_14.xlsx"),
                      sheet = "UserDB_Main_table")


# ========================================================================================
# PROCESS --------------------------------------------------------------------------------
# ========================================================================================

# Clean names and remove empty rows
fct <- fct_raw %>%
  clean_names() %>%
  remove_empty()

# Remove redundant rows
fct <- fct[-1,]
fct <- fct %>%
  filter(!is.na(foodname_in_english))

# Split several columns that contain multiple variables
fct <- fct %>%
  separate(enerc_kcal_k_j, into = c("enerc_kcal", "enerc_kj"), sep = "\\)") %>%
  mutate(enerc_kcal = gsub("\\(", "", enerc_kcal))

# Note that several colums contain [] because they contain a possible other conversion factor:
# cartbeq_or_cartb_mcg, vite_or_tocpha_mg, niaeq_or_nia_mg
# We ignore them for now
# Convert all character strings to numbers
fct <- fct %>%
  mutate(across(edible:ascl_mg, as.numeric))


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

write_csv(fct, file.path(param$db_path, "food_composition_tables/BGD/food_composition_table_BGD.csv"))
