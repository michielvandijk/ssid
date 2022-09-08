# ========================================================================================
# Project:  sidd
# Subject:  process worldpop age sex maps
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here("working_paper/scripts/model_setup/set_model_parameters.r"))



# ========================================================================================
# EXTRACT WORLDPOP AGE-SEX DATA FOR BASE YEAR --------------------------------------------
# ========================================================================================

# CLIP FILES -----------------------------------------------------------------------------
# global age-sex files
files <- list.files(file.path(param$db_path, "age_sex_structures/2018"),
                    pattern = c(".*\\.tif$"), recursive = TRUE, full.names = T)

# Clip file
adm <- st_read(file.path(param$model_path, glue("adm/adm1_{param$iso3c}.shp")))

# Function to age-sex structures
clip_urban_rural <- function(f, adm_file, path = "benchmark/spatial_ssp_population", param){
  sex <- str_split(basename(r), pattern = "_")[[1]][[2]]
  age <- str_split(basename(r), pattern = "_")[[1]][[3]]
  year <- str_split(basename(r), pattern = "_")[[1]][[4]]
  output_folder <- file.path(param$model_path, path)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  file_name <- glue::glue("{sex}_{age}_{param$year}_{param$iso3c}.tif")
  output_file <- glue::glue("{output_folder}/{file_name}")
  cat(basename(output_file), "\n")
  r <- clip_country(f, adm_file, f)
  plot(r, main = basename(output_file))
  writeRaster(r, filename = output_file)
}

# # Clip all files
clip_age_sex(files[4], adm, param = param)
walk(files, clip_country, clip_file = clip_file, iso3c = param$iso3c)


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
age_sex_raw <- map_df(files, aggregate_age_sex, adm) %>%
  arrange(adm1_name, sex, age, year)




# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "projections"), recursive = TRUE, showWarnings = FALSE)
saveRDS(age_sex_raw, file.path(proc_path,
                               glue("benchmark/subnat_age_sex_by_raw_{iso3c_sel}.rds")))


