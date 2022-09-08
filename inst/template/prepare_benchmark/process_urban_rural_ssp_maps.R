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
# FUNCTIONS ------------------------------------------------------------------------------
# ========================================================================================

# Function to clip country map
clip_country <- function(clip_file, iso3c, input_file){
  ssp <- str_split(basename(input_file), pattern = "_")[[1]][[1]]
  output_folder <- file.path(proc_path, glue("benchmark/spatial_ssp_population/{ssp}"))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  file_name <- glue("{strsplit(basename(input_file), '[.]')[[1]][1]}_{iso3c}.tif")
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
      scenario = strsplit(basename(r), '[_/|.]')[[1]][1],
      urban_rural = strsplit(basename(r), '[_/|.]')[[1]][2],
      year = as.integer(strsplit(basename(r), '[_/|.]')[[1]][3]),
      value = exact_extract(raster(r), adm, fun = "sum"))
  return(df)
}


# ========================================================================================
# EXTRACT URBAN-RURAL SSP PROJECTIONS ----------------------------------------------------
# ========================================================================================

# CLIP FILES -----------------------------------------------------------------------------
# Urban-rural data files - do not select total pop files
files <- list.files(file.path(param$db_path, "spatial_ssp_population/"),
                            c("*_(urban|rural)_[0-9]{4}[.]tif$"), recursive = TRUE, full.names = T)


# Clip file
adm <- file.path(param$model_path, glue("adm/adm_{param$iso3c}.shp"))

# Function to clip urban-rural ssp
clip_urban_rural <- function(f, adm_file, path = "benchmark/spatial_ssp_population", param){
  ssp <- str_split(basename(f), pattern = "_")[[1]][[1]]
  output_folder <- file.path(param$model_path, glue("{path}/{ssp}"))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  file_name <- glue("{strsplit(basename(f), '[.]')[[1]][1]}_{iso3c}.tif")
  output_file <- glue("{output_folder}/{file_name}")
  cat(basename(output_file), "\n")
  r <- clip_country(f, adm_file, f)
  plot(r, main = basename(output_file))
  writeRaster(r, filename = output_file)
}


# Clip all files
walk(files[1], clip_urban_rural, adm_file = adm, param = param)


# AGGREGATE OVER ADM -------------------------------------------------------------------
# Adm
adm <- readRDS(file.path(proc_path, glue("adm/adm_{iso3c_sel}.rds")))

# Population projections
pop_files <- list.files(file.path(proc_path, "benchmark/spatial_ssp_population/"),
                        pattern = glob2rx("*.tif"), recursive = TRUE, full.names = T)

# Aggregate over adm
subnat_urban_rural_proj_raw <- map_df(pop_files, ag_adm, adm) %>%
  arrange(adm2_name, scenario, urban_rural, year)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(subnat_urban_rural_proj_raw, file.path(proc_path,
                                          glue("benchmark/subnat_urban_rural_proj_raw_{iso3c_sel}.rds")))


