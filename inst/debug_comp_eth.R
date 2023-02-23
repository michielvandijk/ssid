# ========================================================================================
# Project:  sidd
# Subject:  debug
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Seed
hh_db <- readRDS(file.path(param$model_path, glue("seed/hh_db_{param$iso3c}.rds")))
per_db <- readRDS(file.path(param$model_path, glue("seed/per_db_{param$iso3c}.rds")))

# Adm_list
adm_list <- readRDS(file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))

# Subnat urban-rural projections
subnat_urban_rural_proj <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_urban_rural_proj_{param$iso3c}.rds")))
subnat_urban_rural_proj_m15 <- readRDS(file.path(param$model_path,
                                                 glue("benchmark/subnat_urban_rural_proj_m15_{param$iso3c}.rds")))

# Subnat age_sex projections
subnat_age_sex_proj <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_age_sex_proj_{param$iso3c}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_age_sex_proj_m15_{param$iso3c}.rds")))

# Subnat occupation projections
subnat_occ_proj <- readRDS(file.path(param$model_path,
                                     glue("benchmark/subnat_occupation_proj_{param$iso3c}.rds")))
subnat_occ_proj_m15 <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_occupation_proj_m15_{param$iso3c}.rds")))

# Subnat household projections
subnat_hh_proj <- readRDS(file.path(param$model_path,
                                    glue("benchmark/subnat_hh_proj_{param$iso3c}.rds")))
subnat_hh_proj_m15 <- readRDS(file.path(param$model_path,
                                        glue("benchmark/subnat_hh_proj_m15_{param$iso3c}.rds")))


# ========================================================================================
# PREPARE -------------------------------------------------------------------------------
# ========================================================================================

# Create region - target zone id
adm_list <- adm_list %>%
  dplyr::select(target_zone = adm2_code, region = adm1_name) %>%
  distinct() %>%
  mutate(reg_tz = paste(region, target_zone, sep = "-x-"))

# check totals
subnat_age_sex_proj %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(value),
            .groups = "drop")

subnat_age_sex_proj_m15 %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(value),
            .groups = "drop")

subnat_hh_proj %>%
  group_by(year, scenario) %>%
  summarize(hh = sum(value),
            .groups = "drop")


# ========================================================================================
# BASE YEAR SIMULATION -------------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# For the base year, we include the m15 category
# hh seed
hh_seed_by <- hh_db %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

per_seed_by <- per_db %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation)


# ========================================================================================
# BASE YEAR SIMULATION -------------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# For the base year, we include the m15 category
# hh seed
hh_seed_by <- hh_db %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

per_seed_by <- per_db %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation)


# BENCHMARK --------------------------------------------------------------------------------

bm_by <- prepare_benchmark(subnat_hh_proj, subnat_urban_rural_proj, subnat_age_sex_proj, subnat_occ_proj)


# SIMULATION -------------------------------------------------------------------------------

# Note that some zones do not converge after 100 iterations.
# We adjusted the relative gap from 0.01 to 0.05 and set iterations to 500,
# We also slightly modified the min and max ratio
# which leads to convergence for most regions. We might adjust in the final run
# 7023 sec

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$base_year, stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

adm_sel <- adm_list %>%
  filter(region == "BENISHANGUL-GUMUZ")

tic()
library(ipfr)
sim_by <- map(ssp_y$ssp_y[1], reweigh, adm_sel$reg_tz[3], hh_seed_by, per_seed_by, bm_by,
              verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
              absolute_diff = 10)
names(sim_by) <- ssp_y$ssp_y
toc()


# RUN  BENISHANGUL-GUMUZ-x-23100600 only
bm <- bm_by
reg_tz <- adm_list$reg_tz[1]
hh_s <- hh_seed_by
per_s <- per_seed_by
reg_sample = TRUE
verbose = TRUE
max_iter = 300
max_ratio = 5
min_ratio = 0.2
relative_gap = 0.01
absolute_gap = 10

# Iterative proportional fitting at hh and person level
ipf_seed <- function(reg_tz, hh_s, per_s, bm, p, reg_sample = FALSE, verbose = FALSE,
                     max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                     relative_gap = 0.01, absolute_gap = 10){

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

  sim <- ipu(hh_s_r, hh_t_r, per_s_r, per_t_r, max_iterations = max_iter, verbose = verbose,
             max_ratio = max_ratio, min_ratio = min_ratio, relative_gap = relative_gap)
  return(sim)
}

