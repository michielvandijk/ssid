# ========================================================================================
# Project:  People on the map
# Subject:  Script to download country polygon
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
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue", "raster", "sf", "rmapshaper")

# Set path
source(here(glue("scripts/set_path.r")))

# R options
options(scipen = 999) # Suppress scientific notation
options(digits = 4)



# ========================================================================================
# SET ISO3C ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"


# ========================================================================================
# DOWNLOAD MAP ---------------------------------------------------------------------------
# ========================================================================================

# Download GADM adm0 for ETH using raster
adm0 <- getData(name = "GADM", country = iso3c_sel, 
                level = 0, 
                path = file.path(proc_path, glue("adm")))

adm0 <- getData(name = "GADM", country = iso3c_sel, 
                level = 1, 
                path = file.path(proc_path, glue("adm")))

adm2 <- getData(name = "GADM", country = iso3c_sel, 
                level = 2, 
                path = file.path(proc_path, glue("adm")))

adm3 <- getData(name = "GADM", country = iso3c_sel, 
                level = 3, 
                path = file.path(proc_path, glue("adm")))


# ========================================================================================
# SIMPLIFY MAPS FOR EASY PLOTTING --------------------------------------------------------
# ========================================================================================

# Simplify map for faster processing
adm0 <- st_as_sf(adm0)
adm0_simple <- ms_simplify(adm0, keep = 0.1, keep_shapes = TRUE)
plot(adm0_simple$geometry)

adm1 <- st_as_sf(adm1)
adm1_simple <- ms_simplify(adm1, keep = 0.1, keep_shapes = TRUE)
plot(adm1_simple$geometry)

adm2 <- st_as_sf(adm2)
adm2_simple <- ms_simplify(adm2, keep = 0.1, keep_shapes = TRUE)
plot(adm2_simple$geometry)

adm3 <- st_as_sf(adm3)
adm3_simple <- ms_simplify(adm3, keep = 0.1, keep_shapes = TRUE)
plot(adm3_simple$geometry)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(adm0, file.path(proc_path, glue("adm/gadm36_ETH_0_s.rds")))
saveRDS(adm1, file.path(proc_path, glue("adm/gadm36_ETH_1_s.rds")))
saveRDS(adm2, file.path(proc_path, glue("adm/gadm36_ETH_2_s.rds")))
saveRDS(adm3, file.path(proc_path, glue("adm/gadm36_ETH_3_s.rds")))
