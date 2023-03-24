# ========================================================================================
# Project:  ssid
# Subject:  Script to create adm maps for analysis
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load required packages
p_load(ssid, broom, countrycode, dotwhisker, here, ggspatial, glue, haven, imputeTS, ipumsr, janitor, furrr, progressr,
       terra, tidyverse, readxl, scales, sf, texreg, tictoc)


# R options
options(scipen = 999) # Suppress scientific notation
options(digits = 4)


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================




# Set the folders where the scripts, model and database will be stored.
# Note that R uses forward slashes even in Windows!!

# Creates a model folder structure in c:/temp/ with the name 'ssid_eth'.
# the user can replace eth with the country code of the case-study country or
# choose a new name.
model_path <- "c:/Users/dijk158/OneDrive - Wageningen University & Research/Projects/ssid/ssid_eth"

# Set location of ssid_db
db_path <-   "c:/Users/dijk158/OneDrive - Wageningen University & Research/data/microsim_db"

# Set ssid parameters
param <- ssid_par(
  model_path = model_path,
  db_path = db_path,
  iso3c = "ETH",
  base_year = 2018,
  start_year = 2018,
  end_year = 2050,
  adm_level = 2)

print(param)


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


# ========================================================================================
# RUN ONE REGION, PER YEAR, PER SSP ------------------------------------------------------
# ========================================================================================

temp_path <- file.path(param$model_path, glue("simulation/{Sys.Date()}"))
dir.create(temp_path)

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$base_year, stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

adm_list <- adm_list %>%
  filter(region %in% c("BENISHANGUL-GUMUZ", "ADDIS ABABA", "HARARI", "DIRE DAWA", "GAMBELA"))


tic()
library(ipfr)
sim_by <- ssp_y$ssp_y[c(1)] %>%
  set_names() %>%
  map(reweigh, adm_list$reg_tz, hh_seed_by, per_seed_by, bm_by,
              verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
              absolute_diff = 10, parallel = TRUE, output = temp_path)
toc()




# ========================================================================================
# CONVERGENCE STATISTICS -----------------------------------------------------------------
# ========================================================================================

# Convergence
sim_by_stats <- sim_stats(sim_by, ssp_y$ssp_y[1], adm_list$reg_tz)
table(sim_by_stats$converged)

# plot of weights

# UPDATE: Create function that compares across years per region?
# Create pdf to inspect weights of histogram
pdf(file = file.path(temp_path, "ssp1_3_by_weights.pdf")) # , width = 8.27, height = 11.69
cc(sim, ssp_y$ssp_y, adm_list$reg_tz)
dev.off()


cc <- function(sim, ssp_y, reg_tz){
  stats <- ssp_y %>%
    set_names() %>%
    map(function(y) {
      reg_tz %>%
        set_names() %>%
        map(function(x) {
          cat(x, y)
          p <- sim[[y]][[x]]$weight_dist
          t <- paste(y, x, sep = "_")
          cat(t)
          p
        })
    })
  return(stats)
}

sim[[ssp_y$ssp_y[1]]][[adm_list$reg_tz[1]]]$weight_dist
sim[[y]][[x]]

extract_output <- function(sim, output) {
   out <- map_df(sim, ~map_df(.x,  output, id = "name"), id = "name2")
  return(out)
}
sim$ssp1_2018$`BENISHANGUL-GUMUZ-x-231006001`$
names(sim)
z <- extract_output(sim, "stats")

extract_output <- function(df, var){
  map_df(df, ~map_df(.x, var, .id = "x"), .id = "y")
}

extract_weight_plots <- function(sim, ssp_y, reg_tz) {
  for(i in ssp_y){
    for(j in reg_tz){
      n <- paste(i, j, sep = " ")
      cat(n,"\n")
      print(sim[[i]][[j]]$weight_dist +
              labs(title = n))
    }
  }
}


x <- map_df(sim, ~map_df(.x, "weight_tbl", .id = "name"), .id = "name2")

sim[[ssp_y$ssp_y[1]]][[adm_list$reg_tz[1]]]$weight_dist
mtcars_nested <- mtcars_nested %>%
  mutate(model = map(data, function(df) lm(mpg ~ wt, data = df)))




pdf(file = file.path(temp_path, "weights.pdf")) # , width = 8.27, height = 11.69
extract_plots(sim_by, ssp_y$ssp_y, adm_list$reg_tz)
dev.off()


sim$ssp1_2018$`BENISHANGUL-GUMUZ-x-231006001`$weight_dist


# ========================================================================================
# BASE YEAR SIMULATION -------------------------------------------------------------------
# ========================================================================================


# BENCHMARK --------------------------------------------------------------------------------

bm_by <- prepare_benchmark(subnat_hh_proj, subnat_urban_rural_proj, subnat_age_sex_proj, subnat_occ_proj)


# SIMULATION -------------------------------------------------------------------------------

# Note that some zones do not converge after 100 iterations.
# We adjusted the relative gap from 0.01 to 0.05 and set iterations to 500,
# We also slightly modified the min and max ratio
# which leads to convergence for most regions. We might adjust in the final run
# 1023 sec
#10824
ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$base_year, stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
library(ipfr)
sim_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz, hh_seed_by, per_seed_by, bm_by,
              verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
              absolute_diff = 10, output = temp_path)
names(sim_by) <- ssp_y$ssp_y
toc()

# CHECK CONVERGENCE ----------------------------------------------------------------------

x <- extract_output(sim_by, "weight_tbl")
table(sim_by_stats$converged)

names(sim_by)
sim_by$ssp1_2018$`TIGRAY-x-231001001`

out <- map_df(sim_by, \(x) map(x, "weight_tbl", .id = "ssp_y"), .id = "reg_tz")

xxx <- pluck(sim_by, 1, 1, 1)
xx <- map_df(sim_by, list(c(2), "weight_tbl"))
names(x)
sim_by_stats <- map_df(ssp_y$ssp_y, function(y)
  map_df(seq_along(adm_list$reg_tz), function(x){
    sim_by[[y]][[adm_list$reg_tz[x]]]$stats$stats_sum},
    .id = "reg_tz"
  ), .id = "ssp_y"
)


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
bm_no_by <- prepare_benchmark(subnat_hh_proj_m15, subnat_urban_rural_proj_m15,
                              subnat_age_sex_proj_m15, subnat_occ_proj_m15)


# SIMULATION ------------------------------------------------------------------------------

# Takes # 87389.17 sec elapsed no parallel
ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"),
                     y = c(2020, 2030, 2040, 2050), stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
sim_no_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz, hh_seed_no_by, per_seed_no_by, bm_no_by,
                 verbose = FALSE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
                 absolute_diff = 10, output = temp_path)
names(sim_no_by) <- ssp_y$ssp_y
toc()




# CHECK CONVERGENCE ----------------------------------------------------------------------
sim_no_by_stats <- map_df(seq_along(adm_list$reg_tz), function(x) {sim_no_by$ssp2_2050[[adm_list$reg_tz[x]]]$stats$stats_sum})
table(sim_no_by_stats$converged)

sim_no_by_stats <- map_df(ssp_y$ssp_y, function(y)
  map_df(seq_along(adm_list$reg_tz), function(x){
    sim_no_by[[y]][[adm_list$reg_tz[x]]]$stats$stats_sum},
    .id = "reg_tz"
  ), .id = "ssp_y"
)

