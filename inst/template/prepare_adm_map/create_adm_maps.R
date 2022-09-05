# ========================================================================================
# Project:  simFNS
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

# Load key packages
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue")

# Load additional packages
p_load("ipumsr", "sf", "fuzzyjoin", "mapview", "raster", "countrycode")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999) # Suppress scientific notation
options(digits = 4)
options(viewer = NULL)


# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# IPUMS adm2 world map
adm1_wld <- st_read(file.path(raw_path, "ipums/shapefiles/world_geolev1_2019/world_geolev1_2019.shp"))
adm2_wld <- st_read(file.path(raw_path, "ipums/shapefiles/world_geolev2_2019/world_geolev2_2019.shp"))


# ========================================================================================
# PREPARE MAPS ---------------------------------------------------------------------------
# ========================================================================================

# ADM1 -----------------------------------------------------------------------------------

adm1 <- adm1_wld %>%
  filter(CNTRY_CODE == paste0(countrycode(iso3c_sel, "iso3c", "un"))) %>%
  transmute(adm0_code = iso3c_sel, 
            adm1_name = ADMIN_NAME, 
            adm1_code = GEOLEVEL1) %>%
  st_make_valid()

# Fix by removing empty geography and Lake Tana
adm1 <- adm1 %>%
  filter(!adm1_name %in% c("Lake Tana", "Special Region"))

# Recode names with special characters, mistakes or which are too long
adm1 <- adm1 %>%
  mutate(adm1_name =  case_when(
    adm1_name == "Benishangul-Gumz\r\n" ~ "Benishangul-Gumuz",
    adm1_name == "Southern Nations, Nationalities, and People (SNPP)" ~ "SNNP",
    TRUE ~ adm1_name)
  )

# ADM2 -----------------------------------------------------------------------------------

adm2 <- adm2_wld %>%
  filter(CNTRY_CODE == paste0(countrycode(iso3c_sel, "iso3c", "un"))) %>%
  transmute(adm0_code = iso3c_sel, 
            adm2_name = ADMIN_NAME, 
            adm2_code = GEOLEVEL2) %>%
  st_make_valid() # added to solve potential problems with 'Ring Self-intersection'

# Link adm1 code and name
adm2 <- adm2 %>%
  mutate(adm1_code = str_sub(adm2_code, 1, nchar(adm2_code)-3)) %>%
  left_join(st_drop_geometry(adm1)) %>%
  dplyr::select(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)

# Fix by removing empty geography and Lake Tana
adm2 <- adm2 %>%
  filter(!adm2_name %in% c("Lake Tana", "Special EA 1", "Special EA 2", "Special EA 3"))


# ADM LIST -------------------------------------------------------------------------------

adm_list <- adm2 %>%
  st_drop_geometry()

# DETERMINE TARGET ADM -------------------------------------------------------------------
# We run the model at adm2
adm <- adm2


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "adm"), showWarnings = FALSE, recursive = TRUE)
saveRDS(adm_list, file.path(proc_path, glue("adm/adm_list_{iso3c_sel}.rds")))
saveRDS(adm, file.path(proc_path, glue("adm/adm_{iso3c_sel}.rds")))
st_write(adm, file.path(proc_path, glue("adm/adm_{iso3c_sel}.shp")), delete_dsn = TRUE)
saveRDS(adm1, file.path(proc_path, glue("adm/adm1_{iso3c_sel}.rds")))
saveRDS(adm2, file.path(proc_path, glue("adm/adm2_{iso3c_sel}.rds")))


