# ========================================================================================
# Project:  simFNS
# Subject:  Reweighing of seed
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
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue", "ipfr", "furrr",
       "progressr", "tictoc")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# FUNCTIONS ------------------------------------------------------------------------------
# ========================================================================================

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

# Function to prepare benchmark, including all scenarios and years
prep_benchmark <- function(hh, u_r, a_s, occ){

  # Extract hh_number for all zones
  hh_number <- hh %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, n = value) %>%
    distinct()

  # Extract per targets for all zones
  per_urban_rural <- u_r %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, urban_rural, scenario, year, value) %>%
    distinct() %>%
    pivot_wider(names_from = urban_rural, values_from = value, values_fill = 0)

  per_age <- a_s %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, age, value) %>%
    group_by(scenario, year, target_zone, region, age) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = age, values_from = value, values_fill = 0)

  per_sex <- a_s %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, sex, value) %>%
    group_by(scenario, year, target_zone, region, sex) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = sex, values_from = value, values_fill = 0)

  per_occ <- occ %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, variable, value) %>%
    group_by(scenario, year, target_zone, region, variable) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = variable, values_from = value, values_fill = 0)

  hh_bm <- list(hh_number = hh_number)
  per_bm <- list(
    per_urban_rural = per_urban_rural,
    per_age = per_age,
    per_sex = per_sex,
    per_occ = per_occ)
  bm <- list(hh_bm = hh_bm, per_bm = per_bm)
  return(bm)
}

# Function to select ssp and year from benchmark
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


# Create weights for pdf
create_weights_pdf <- function(reg_tz, ss){
  cat(reg_tz, "\n")
  nms <- names(ss)
  h <- ss[[reg_tz]]$weight_dist
  print(h + labs(title = reg_tz))
}

# Loop over year and ssp combinations
reweigh <- function(ssp_y, reg_tz, hh_s, per_s, bm,
                    verbose = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_gap = 10){
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
                 min_ratio = min_ratio, relative_gap = relative_gap)
  })
  plan(sequential)
  return(sim)
}

# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Seed
hh_db <- readRDS(file.path(proc_path, glue("simulation/hh_db_{iso3c_sel}.rds")))
per_db <- readRDS(file.path(proc_path, glue("simulation/per_db_{iso3c_sel}.rds")))

# Adm_list
adm_list <- readRDS(file.path(proc_path, glue("adm/adm_list_{iso3c_sel}.rds")))

# Subnat urban-rural projections
subnat_urban_rural_proj <- readRDS(file.path(proc_path,
                                             glue("benchmark/subnat_urban_rural_proj_{iso3c_sel}.rds")))
subnat_urban_rural_proj_m15 <- readRDS(file.path(proc_path,
                                             glue("benchmark/subnat_urban_rural_proj_m15_{iso3c_sel}.rds")))

# Subnat age_sex projections
subnat_age_sex_proj <- readRDS(file.path(proc_path,
                                       glue("benchmark/subnat_age_sex_proj_{iso3c_sel}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(proc_path,
                                         glue("benchmark/subnat_age_sex_proj_m15_{iso3c_sel}.rds")))

# Subnat occupation projections
subnat_occ_proj <- readRDS(file.path(proc_path,
                                   glue("benchmark/subnat_occupation_proj_{iso3c_sel}.rds")))
subnat_occ_proj_m15 <- readRDS(file.path(proc_path,
                                     glue("benchmark/subnat_occupation_proj_m15_{iso3c_sel}.rds")))

# Subnat household projections
subnat_hh_proj <- readRDS(file.path(proc_path,
                                  glue("benchmark/subnat_hh_proj_{iso3c_sel}.rds")))
subnat_hh_proj_m15 <- readRDS(file.path(proc_path,
                                    glue("benchmark/subnat_hh_proj_m15_{iso3c_sel}.rds")))


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
  summarize(pop = sum(value)) 

subnat_age_sex_proj_m15 %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(value)) 

subnat_hh_proj %>%
  group_by(year, scenario) %>%
  summarize(hh = sum(value)) 


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

bm_by <- prep_benchmark(subnat_hh_proj, subnat_urban_rural_proj, subnat_age_sex_proj, subnat_occ_proj)


# SIMULATION -------------------------------------------------------------------------------

# Note that some zones do not converge after 100 iterations.
# We adjusted the relative gap from 0.01 to 0.05 and set iterations to 500,
# which leads to convergence for most regions. We might adjust in the final run
# 7023 sec
ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = c(2018), stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
sim_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz, hh_seed_by, per_seed_by, bm_by,
               verbose = TRUE, max_iter = 500, max_ratio = 5, min_ratio = 0.2, relative_gap = 0.05,
               absolute_gap = 10)
names(sim_by) <- ssp_y$ssp_y
toc()

# CHECK CONVERGENCE ----------------------------------------------------------------------

sim_by_stats <- map_df(seq_along(adm_list$reg_tz), function(x) {sim_by$ssp2_2018[[adm_list$reg_tz[x]]]$stats$stats_sum})
table(sim_by_stats$converged)


# ========================================================================================
# NOT BY SIMULATION ----------------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# We exclude any families that only consist of m15 members. If not, ipu cannot solved as
# there is no matching data in per seed. In this case n = 1
hh_seed_no_by <- hh_db %>%
  filter(hh_size != hh_n_m15) %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

# We exclude the m15 category and distribute the number of children post process
per_seed_no_by <- per_db %>%
  rename(region = adm1_name) %>%
  filter(age != "m15") %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation) %>%
  droplevels()


# BENCHMARK -------------------------------------------------------------------------------

# We use the projections that exclude the m15 class
bm_no_by <- prep_benchmark(subnat_hh_proj_m15, subnat_urban_rural_proj_m15,
                          subnat_age_sex_proj_m15, subnat_occ_proj_m15)


# SIMULATION ------------------------------------------------------------------------------

# Takes around 1987.47 sec ~ 30 min.
ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"),
                     y = c(2018, 2020, 2030, 2040, 2050), stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
sim_no_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz, hh_seed_no_by, per_seed_no_by, bm_no_by,
                verbose = TRUE, max_iter = 500, max_ratio = 5, min_ratio = 0.2, relative_gap = 0.05,
                absolute_gap = 10)
names(sim_no_by) <- ssp_y$ssp_y
toc()


# CHECK CONVERGENCE ----------------------------------------------------------------------

sim_no_by_stats <- map_df(seq_along(adm_list$reg_tz), function(x) {sim_no_by$ssp2_2050[[adm_list$reg_tz[x]]]$stats$stats_sum})
table(sim_no_by_stats$converged)


# ========================================================================================
# COMBINE SIMULATIONS --------------------------------------------------------------------
# ========================================================================================

# TO DO: Convert all sims to data.frames with columns that include scenario and year and bind.
# In this way, all simulations can quickly be filtered, etc.


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

temp_path <- file.path(proc_path, glue("simulation/{Sys.Date()}"))
dir.create(temp_path, recursive = T, showWarnings = F)
saveRDS(sim_by, file.path(temp_path, glue("ssp1_3_by_{iso3c_sel}.rds")))
saveRDS(sim_no_by, file.path(temp_path, glue("ssp1_3_no_by_{iso3c_sel}.rds")))


# ========================================================================================
# PLOT WEIGHTS ---------------------------------------------------------------------------
# ========================================================================================

# UPDATE: Create function that compares across years per region?
# Create pdf to inspect weights of histogram
pdf(file = file.path(temp_path, "ssp1_3_by_weights.pdf")) # , width = 8.27, height = 11.69
walk(adm_list$reg_tz, create_weights_pdf, ss = sim_by[[1]])
dev.off()

pdf(file = file.path(temp_path, "ssp1_3_2050_weights.pdf")) # , width = 8.27, height = 11.69
walk(adm_list$reg_tz, create_weights_pdf, ss = sim_no_by[[1]])
dev.off()


