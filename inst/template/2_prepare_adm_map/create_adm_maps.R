# ========================================================================================
# Project:  sidd
# Subject:  Script to create adm map
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here("working_paper/scripts/model_setup/set_model_parameters.r"))

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

# We do not use adm3
adm_list <- adm2 %>%
  st_drop_geometry() %>%
  unique()


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

# NOTE -----------------------------------------------------------------------------------

# Save map with all relevent adm levels as adm_iso3c.

dir.create(file.path(param$model_path, "adm"), showWarnings = FALSE, recursive = TRUE)
write_csv(adm_list, file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.csv")))
saveRDS(adm2, file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))

# saveRDS(adm1, file.path(param$model_path, glue("adm/adm1_{param$iso3c}.rds")))
# st_write(adm1, file.path(param$model_path, glue("adm/adm1_{param$iso3c}.shp")), delete_dsn = TRUE)
# saveRDS(adm2, file.path(param$model_path, glue("adm/adm2_{param$iso3c}.rds")))
# saveRDS(adm3, file.path(param$model_path, glue("adm/adm3_{param$iso3c}.rds")))


# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(adm_list, adm1, adm1_raw, adm2, adm2_raw, adm3, adm3_raw)
