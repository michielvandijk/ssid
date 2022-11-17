# ========================================================================================
# Project:  ssid
# Subject:  Script to create adm map
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("working_paper/scripts/1_model_setup/set_model_parameters.r"))

# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Map from https://data.humdata.org/dataset/cod-ab-bgd
adm1_raw <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/bgd_adm_bbs_20201113_SHP/bgd_admbnda_adm1_bbs_20201113.shp")))
adm2_raw <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/bgd_adm_bbs_20201113_SHP/bgd_admbnda_adm2_bbs_20201113.shp")))
adm3_raw <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/bgd_adm_bbs_20201113_SHP/bgd_admbnda_adm3_bbs_20201113.shp")))


# ========================================================================================
# PREPARE MAPS ---------------------------------------------------------------------------
# ========================================================================================

adm1 <- adm1_raw %>%
  transmute(adm0_code = param$iso3c, adm1_name = toupper(ADM1_EN), adm1_code = ADM1_PCODE)

adm2 <- adm2_raw %>%
  transmute(adm0_code = param$iso3c, adm1_name = toupper(ADM1_EN), adm1_code = ADM1_PCODE,
            adm2_name = toupper(ADM2_EN), adm2_code = ADM2_PCODE)

adm3 <- adm3_raw %>%
  transmute(adm0_code = param$iso3c, adm1_name = toupper(ADM1_EN), adm1_code = ADM1_PCODE,
            adm2_name = toupper(ADM2_EN), adm2_code = ADM2_PCODE, adm3_name = toupper(ADM3_EN),
            adm3_code = ADM3_PCODE)

# Prepare adm list, which includes adm level at which the model is run and potential higher level adm
# that might me used for subsampling in the spatial microsimulation.
adm_list <- adm2 %>%
  st_drop_geometry() %>%
  unique()

# We use the adm2 map for data processing
adm <- adm2


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

# NOTE -----------------------------------------------------------------------------------

# Save map with all relevant adm levels as adm_iso3c.

dir.create(file.path(param$model_path, "adm"), showWarnings = FALSE, recursive = TRUE)
saveRDS(adm_list, file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))
saveRDS(adm, file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))


# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(adm_list, adm, adm1, adm1_raw, adm2, adm2_raw, adm3, adm3_raw)
