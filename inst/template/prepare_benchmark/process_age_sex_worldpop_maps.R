# ========================================================================================
# Project:  simFNS
# Subject:  process worldpop age sex maps
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
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue", "janitor", "countrycode",
       "gdalUtils", "sf", "raster", "exactextractr")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# FUNCTIONS ------------------------------------------------------------------------------
# ========================================================================================

# Function to clip country map
clip_country <- function(clip_file, iso3c, input_file){
  sex <- str_split(basename(input_file), pattern = "_")[[1]][[2]]
  age <- str_split(basename(input_file), pattern = "_")[[1]][[3]]
  year <- str_split(basename(input_file), pattern = "_")[[1]][[4]]
  output_folder <- file.path(proc_path, glue("benchmark/worldpop_age_sex"))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  file_name <- glue("{sex}_{age}_{year}_{iso3c}.tif")
  output_file <- glue("{output_folder}/{file_name}")
  cat(basename(output_file), "\n")
  output_map <- gdalUtilities::gdalwarp(srcfile = input_file, dstfile = output_file,
                                        cutline = clip_file, crop_to_cutline = T,
                                        r = "near", overwrite = T)
  plot(raster(output_map), main = basename(output_file))
}

# function to aggregate population at adm level
ag_adm <- function(r, adm_file){
  cat(basename(r), "\n")
  df <- adm_file %>%
    st_drop_geometry() %>%
    mutate(
      sex = strsplit(basename(r), '[_]')[[1]][1],
      age = strsplit(basename(r), '[_]')[[1]][2],
      year = as.integer(strsplit(basename(r), '[_]')[[1]][3]),
      value = exact_extract(raster(r), adm, fun = "sum"))
  return(df)
}


# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"


# ========================================================================================
# EXTRACT WORLDPOP AGE-SEX DATA FOR BASE YEAR --------------------------------------------
# ========================================================================================

# CLIP FILES -----------------------------------------------------------------------------
# global age-sex files
files <- list.files(file.path(raw_path, "worldpop/global_age_sex_2018"),
                    pattern = c(".*\\.tif$"), recursive = TRUE, full.names = T)

# Clip file
clip_file <- file.path(proc_path, glue("adm/adm_{iso3c_sel}.shp"))

# # Clip all files
walk(files, clip_country, clip_file = clip_file, iso3c = iso3c_sel)


# AGGREGATE OVER ADM ---------------------------------------------------------------------
# Adm
adm <- readRDS(file.path(proc_path, glue("adm/adm_{iso3c_sel}.rds")))

# Clipped age_sex files
files <- list.files(file.path(proc_path, "benchmark/worldpop_age_sex/"),
                    pattern = c(".*\\.tif$"), recursive = TRUE, full.names = T)

# Aggregate over adm
age_sex_raw <- map_df(files, ag_adm, adm) %>%
  arrange(adm2_name, sex, age, year)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "projections"), recursive = TRUE, showWarnings = FALSE)
saveRDS(age_sex_raw, file.path(proc_path,
                               glue("benchmark/subnat_age_sex_by_raw_{iso3c_sel}.rds")))


