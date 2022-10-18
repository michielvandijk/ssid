# ========================================================================================
# Project:  ssid
# Subject:  Reweighing of seed
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("inst/debug.r"))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Seed
hh_db <- readRDS(file.path(param$model_path, glue("simulation/hh_db_{param$iso3c}.rds")))
per_db <- readRDS(file.path(param$model_path, glue("simulation/per_db_{param$iso3c}.rds")))

# Adm_list
adm_list <- readRDS(file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))

# Subnat urban-rural projections
subnat_urban_rural_proj_raw <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_urban_rural_proj_{param$iso3c}.rds")))
subnat_urban_rural_proj_m15_raw <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_urban_rural_proj_m15_{param$iso3c}.rds")))

# Subnat age_sex projections
subnat_age_sex_proj_raw <- readRDS(file.path(param$model_path,
                                       glue("benchmark/subnat_age_sex_proj_{param$iso3c}.rds")))
subnat_age_sex_proj_m15_raw <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_age_sex_proj_m15_{param$iso3c}.rds")))

# Subnat occupation projections
subnat_occ_proj_raw <- readRDS(file.path(param$model_path,
                                   glue("benchmark/subnat_occupation_proj_{param$iso3c}.rds")))
subnat_occ_proj_m15_raw <- readRDS(file.path(param$model_path,
                                     glue("benchmark/subnat_occupation_proj_m15_{param$iso3c}.rds")))

# Subnat household projections
subnat_hh_proj_raw <- readRDS(file.path(param$model_path,
                                  glue("benchmark/subnat_hh_proj_{param$iso3c}.rds")))
subnat_hh_proj_m15_raw <- readRDS(file.path(param$model_path,
                                    glue("benchmark/subnat_hh_proj_m15_{param$iso3c}.rds")))


# ========================================================================================
# PREPARE -------------------------------------------------------------------------------
# ========================================================================================

# Create region - target zone id
adm_list <- adm_list %>%
  dplyr::select(target_zone = adm2_code, region = adm1_name) %>%
  distinct() %>%
  mutate(reg_tz = paste(region, target_zone, sep = "-x-"))


# ========================================================================================
# CREATE TEST DATA -----------------------------------------------------------------------
# ========================================================================================
#
# hh_db <- hh_db[c(1:400),]
# summary(hh_db)
# per_db <- filter(per_db, hh_id %in% hh_db$hh_id)
# summary(per_db)

###
#BUKEDI-x-233.1
#"SOUTH BUGANDA-x-105.1"
target_zone <- "233.1"
target_ssp <- "ssp2"
target_year <- 2018
target_region <- adm_list$region[adm_list$target_zone == target_zone]


subnat_hh_proj <- filter(subnat_hh_proj_raw, adm2_code %in% target_zone,
                         scenario %in% target_ssp, year == target_year)

subnat_urban_rural_proj <- filter(subnat_urban_rural_proj_raw, adm2_code %in% target_zone,
                                  scenario %in% target_ssp, year == target_year)

subnat_age_sex_proj <- filter(subnat_age_sex_proj_raw, adm2_code %in% target_zone,
                              scenario %in% target_ssp, year == target_year)

subnat_occ_proj <- filter(subnat_occ_proj_raw, adm2_code %in% target_zone,
                          scenario %in% target_ssp, year == target_year)

# ========================================================================================
# SIMULATION FOR ONE REGIONS -------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# For the base year, we include the m15 category
# hh seed
hh_seed_by <- hh_db %>%
  #rename(region = adm1_name) %>%
  #mutate(region = target_region) %>%
  mutate(region = "UGA") %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

per_seed_by <- per_db %>%
  #rename(region = adm1_name) %>%
  #mutate(region = target_region) %>%
  mutate(region = "UGA") %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation)


# BENCHMARK --------------------------------------------------------------------------------

bm_by_reg <- prepare_benchmark(subnat_hh_proj, subnat_urban_rural_proj, subnat_age_sex_proj, subnat_occ_proj)


# SIMULATION -------------------------------------------------------------------------------

ssp_y_reg <- glue("{target_ssp}_{target_year}")

ssp_y_reg <- "ssp2_2018"
reg_tz_reg <- adm_list$reg_tz[adm_list$target_zone == target_zone & adm_list$region == target_region]

library(ipfr)
sim_by <- map(ssp_y_reg, reweigh, reg_tz_reg, hh_seed_by, per_seed_by, bm_by_reg,
              verbose = TRUE, max_iter = 500, max_ratio = 20, min_ratio = 0.01,
              relative_gap = 0.05, absolute_diff = 10)


# ========================================================================================
# SIMULATION FOR MULTIPLE REGIONS --------------------------------------------------------
# ========================================================================================

# BENCHMARK --------------------------------------------------------------------------------

bm_by <- prepare_benchmark(subnat_hh_proj_raw,
                           subnat_urban_rural_proj_raw,
                           subnat_age_sex_proj_raw,
                           subnat_occ_proj_raw)

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$base_year, stringsAsFactors = FALSE) %>%
   mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
sim_by <- map(ssp_y$ssp_y[1], reweigh, adm_list$reg_tz[c(1:50)], hh_seed_by, per_seed_by, bm_by,
                verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01,
              relative_gap = 0.05, absolute_diff = 10)
names(sim_by) <- ssp_y$ssp_y
toc()



ssp_y <- ssp_y_reg
reg_tz <- reg_tz_reg

# Loop over year and ssp combinations
reweigh2 <- function(ssp_y, reg_tz, hh_s, per_s, bm,
                    verbose = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_diff = 10){
  cat(ssp_y)
  ssp <- str_split(ssp_y, pattern = "_")[[1]][1]
  y <- str_split(ssp_y, pattern = "_")[[1]][2]
  plan(multisession, workers = max(0, availableCores()-4))
  with_progress({
    p <- progressor(steps = length(reg_tz))
    sim <- reg_tz %>%
      set_names %>%
      future_map(ipf_seed, hh_s, per_s, ssid:::select_benchmark(ssp, y, bm), p = p,
                 verbose = verbose, max_iter = max_iter, max_ratio = max_ratio,
                 min_ratio = min_ratio, relative_gap = relative_gap, absolute_diff = absolute_diff               )
  })
  plan(sequential)
  return(sim)
}

debug(reweigh2)
reweigh2(ssp_y_reg, reg_tz_reg, hh_seed_by, per_seed_by, bm_by_reg,
        verbose = TRUE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
        absolute_diff = 10)


# ========================================================================================
# DEBUG ----------------------------------------------------------------------------------
# ========================================================================================

ssp_y <- ssp_y_reg
hh_s <- hh_seed_by
per_s <- per_seed_by
bm <- bm_by_reg
reg_tz <- reg_tz_reg

verbose = FALSE; reg_sample = FALSE; max_iter = 300; max_ratio = 5; min_ratio = 0.2;
relative_gap = 0.01; absolute_diff = 10

reweigh <- function(ssp_y, reg_tz, hh_s, per_s, bm,
                    verbose = FALSE, reg_sample = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_diff = 10){
  cat(ssp_y)
  ssp <- str_split(ssp_y, pattern = "_")[[1]][1]
  y <- str_split(ssp_y, pattern = "_")[[1]][2]
  plan(multisession, workers = max(0, availableCores()-4))
  with_progress({
    p <- progressor(steps = length(reg_tz))
    sim <- reg_tz %>%
      set_names() %>%
      furrr::future_map(ipf_seed, hh_s, per_s, p = p, bm = select_benchmark(ssp, y, bm), reg_sample = reg_sample,
                        verbose = verbose, max_iter = max_iter, max_ratio = max_ratio,
                        min_ratio = min_ratio, relative_gap = relative_gap, absolute_diff = absolute_diff)
  })
  plan(sequential)
  return(sim)
}


reweigh <- function(ssp_y, reg_tz, hh_s, per_s, bm,
                    verbose = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_diff = 10){
  cat(ssp_y)
  ssp <- str_split(ssp_y, pattern = "_")[[1]][1]
  y <- str_split(ssp_y, pattern = "_")[[1]][2]
  plan(multisession, workers = max(0, availableCores()-4))
  with_progress({
    p <- progressor(steps = length(reg_tz))
    sim <- reg_tz %>%
      set_names %>%
      future_map(ipf_seed, hh_s, per_s, select_benchmark(ssp, y, bm), p = p,
                 verbose = verbose, max_iter = max_iter, max_ratio = max_ratio,
                 min_ratio = min_ratio, relative_gap = relative_gap, absolute_diff = absolute_diff)
  })
  plan(sequential)
  return(sim)
}

bm <- ssid:::select_benchmark(ssp, y, bm)
ipf_seed <- function(reg_tz, hh_s, per_s, bm, p, reg_sample = FALSE, verbose = FALSE,
                     max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                     relative_gap = 0.01, absolute_diff = 10){

  # To display progress bar in combination with furrr and progressr
  p()

  prep_target <- function(tar, reg, tz){
    var <- names(tar)[!names(tar) %in% c("target_zone", "region", "scenario", "year")]
    out <- tar[tar$region == reg & tar$target_zone == tz, var]
    return(out)
  }

  reg <- strsplit(reg_tz, split = "-x-")[[1]][1]
  tz <- strsplit(reg_tz, split = "-x-")[[1]][2]
  cat("\n", tz, "in", reg, "\n")

  if(reg_sample){
    hh_s_r <- hh_s[hh_s$region == reg,]
    cat("Subnational seed sample used")
  } else {
    hh_s_r <- hh_s
  }

  hh_s_r <- hh_s_r[names(hh_s_r)[!names(hh_s_r) %in% c("region")]]

  hh_t_r <- list()
  hh_t_r[["hh_number"]] <- bm$hh_bm$hh_number
  hh_t_r <- lapply(hh_t_r, prep_target, reg, tz)

  if(reg_sample){
    per_s_r <- per_s[per_s$region == reg,]
  } else {
    per_s_r <- per_s
  }

  per_s_r <- per_s_r[names(per_s_r)[!names(per_s_r) %in% c("region")]]

  per_t_r <- list()
  per_t_r[["age"]] <- bm$per_bm$per_age
  per_t_r[["sex"]] <- bm$per_bm$per_sex
  per_t_r[["urban_rural"]] <- bm$per_bm$per_urban_rural
  per_t_r[["occupation"]] <- bm$per_bm$per_occ
  per_t_r <- lapply(per_t_r, prep_target, reg, tz)

  sim <- ipfr::ipu(hh_s_r, hh_t_r, per_s_r, per_t_r, max_iterations = max_iter, verbose = verbose,
                   max_ratio = max_ratio, min_ratio = min_ratio, relative_gap = relative_gap, absolute_diff = absolute_diff)
  return(sim)
}

tar <- hh_t_r[[1]]
function(tar, reg, tz){
  var <- names(tar)[!names(tar) %in% c("target_zone", "region", "scenario", "year")]
  out <- tar[tar$region == reg & tar$target_zone == tz, var]
  return(out)
}

all.equal(bm, bm_sel)

select_benchmark <- function(ssp, y, bm){
  hh_number <- bm$hh_bm$hh_number %>%
    filter(year == y, scenario == ssp)
  per_urban_rural <- bm$per_bm$per_urban_rural %>%
    filter(year == y, scenario == ssp)
  per_age <- bm$per_bm$per_age %>%
    filter(year == y, scenario == ssp)
  per_sex <- bm$per_bm$per_sex %>%
    filter(year == y, scenario == ssp)
  per_occ <- bm$per_bm$per_occ %>%
    filter(year == y, scenario == ssp)
  hh_bm <- list(hh_number = hh_number)
  per_bm <- list(
    per_urban_rural = per_urban_rural,
    per_age = per_age,
    per_sex = per_sex,
    per_occ = per_occ)
  bm <- list(hh_bm = hh_bm, per_bm = per_bm)
  return(bm)
}
