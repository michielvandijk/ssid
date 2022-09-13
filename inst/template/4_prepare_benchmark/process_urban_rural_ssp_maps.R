# ========================================================================================
# Project:  simFNS
# Subject:  Extract urban_rural SSP projection maps
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here("working_paper/scripts/model_setup/set_model_parameters.r"))


# ========================================================================================
# EXTRACT URBAN-RURAL SSP PROJECTIONS ----------------------------------------------------
# ========================================================================================

# CLIP FILES -----------------------------------------------------------------------------
# Urban-rural data files - do not select total pop files
files <- list.files(file.path(param$db_path, "spatial_ssp_population/"),
                            c("*_(urban|rural)_[0-9]{4}[.]tif$"), recursive = TRUE, full.names = T)


# adm
adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))

# Function to clip urban-rural ssp
clip_urban_rural <- function(f, adm_file, path = "benchmark/spatial_ssp_population", param){
  ssp <- str_split(basename(f), pattern = "_")[[1]][[1]]
  output_folder <- file.path(param$model_path, glue("{path}/{ssp}"))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  file_name <- glue("{strsplit(basename(f), '[.]')[[1]][1]}_{param$iso3c}.tif")
  output_file <- glue("{output_folder}/{file_name}")
  cat(basename(output_file), "\n")
  r <- clip_country(f, adm_file)
  #plot(r, main = basename(output_file))
  writeRaster(r, filename = output_file, overwrite = TRUE)
}


# Clip all files
walk(files, clip_urban_rural, adm_file = adm, param = param)


# AGGREGATE OVER ADM -------------------------------------------------------------------

# Population projections
files <- list.files(file.path(param$model_path, "benchmark/spatial_ssp_population/"),
                        pattern = glob2rx("*.tif"), recursive = TRUE, full.names = T)

# function to aggregate ssp spatial population to adm and add ssp identifier
aggregate_urban_rural <- function(f, adm_file){
  cat(basename(f), "\n")
  df <- aggregate2adm(f, adm_file) %>%
    mutate(
      scenario = strsplit(basename(f), '[_/|.]')[[1]][1],
      urban_rural = strsplit(basename(f), '[_/|.]')[[1]][2],
      year = as.integer(strsplit(basename(f), '[_/|.]')[[1]][3])
    )
  return(df)
}

# Aggregate over adm
urban_rural_proj_raw <- map_df(files, aggregate_urban_rural, adm)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(urban_rural_proj_raw, file.path(param$model_path,
                                          glue("benchmark/subnat_urban_rural_proj_raw_{param$iso3c}.rds")))


# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(adm, urban_rural_proj_raw, clip_urban_rural, aggregate_urban_rural, files)

