# ========================================================================================
# Project:  ssid
# Subject:  process worldpop age sex maps
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("working_paper/scripts/1_model_setup/set_model_parameters.r"))



# ========================================================================================
# EXTRACT WORLDPOP AGE-SEX DATA FOR BASE YEAR --------------------------------------------
# ========================================================================================

# CLIP FILES -----------------------------------------------------------------------------
# global age-sex files
files <- list.files(file.path(param$db_path, glue("age_sex_structures/{param$base_year}")),
                    pattern = c(".*\\.tif$"), recursive = TRUE, full.names = T)

# adm
adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))

# Function to age-sex structures
clip_age_sex <- function(f, adm_file, path = "benchmark/age_sex_structures", param){
  sex <- str_split(basename(f), pattern = "_")[[1]][[2]]
  age <- str_split(basename(f), pattern = "_")[[1]][[3]]
  year <- str_split(basename(f), pattern = "_")[[1]][[4]]
  output_folder <- file.path(param$model_path, path)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  file_name <- glue::glue("{sex}_{age}_{param$base_year}_{param$iso3c}.tif")
  output_file <- glue::glue("{output_folder}/{file_name}")
  cat(basename(output_file), "\n")
  r <- clip_country(f, adm_file)
  #plot(r, main = basename(output_file))
  writeRaster(r, filename = output_file, overwrite = TRUE)
}

# Clip all files
walk(files, clip_age_sex, adm_file = adm, param = param)


# AGGREGATE OVER ADM ---------------------------------------------------------------------
# Clipped age_sex files
files <- list.files(file.path(param$model_path, "benchmark/age_sex_structures/"),
                    pattern = c(".*\\.tif$"), recursive = TRUE, full.names = T)

# function to aggregate age-sex structures to adm and add age and sex identifier
aggregate_age_sex <- function(f, adm_file){
  cat(basename(f), "\n")
  df <- aggregate2adm(f, adm_file) %>%
    mutate(
       sex = strsplit(basename(f), '[_]')[[1]][1],
      age = strsplit(basename(f), '[_]')[[1]][2],
      year = as.integer(strsplit(basename(f), '[_]')[[1]][3]))
  return(df)
}


# Aggregate over adm
age_sex_raw <- map_df(files, aggregate_age_sex, adm)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(age_sex_raw, file.path(param$model_path,
                               glue("benchmark/subnat_age_sex_by_raw_{param$iso3c}.rds")))



# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(adm, age_sex_raw, clip_age_sex, aggregate_age_sex, files)

