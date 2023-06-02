# ========================================================================================
# Project:  ssid
# Subject:  Reweighing of seed
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

# Seed
hh_db <- readRDS(file.path(param$model_path, glue("seed/seed_hh_db_{param$iso3c}.rds")))
per_db <- readRDS(file.path(param$model_path, glue("seed/seed_per_db_{param$iso3c}.rds")))

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


# BENCHMARK --------------------------------------------------------------------------------

bm_by <- prepare_benchmark(subnat_hh_proj, subnat_urban_rural_proj, subnat_age_sex_proj, subnat_occ_proj)


# SIMULATION -------------------------------------------------------------------------------
temp_path <- file.path(param$model_path, glue("simulation/{Sys.Date()}"))
dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)

# Note that some zones do not converge after 100 iterations.
# We adjusted the relative gap from 0.01 to 0.05 and set iterations to 500,
# We also slightly modified the min and max ratio
# which leads to convergence for most regions. We might adjust in the final run
# 7023 sec

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"), y = param$seed_year, stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
library(ipfr)
sim_by <- ssp_y$ssp_y %>%
  set_names() %>%
  map(reweigh, adm_list$reg_tz, hh_seed_by, per_seed_by, bm_by, param,
      verbose = TRUE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
      absolute_diff = 10, parallel = TRUE, output = temp_path)
names(sim_by) <- ssp_y$ssp_y
toc()





# CHECK CONVERGENCE ----------------------------------------------------------------------

sim_by_stats <- map_df(ssp_y$ssp_y, function(y)
  map_df(seq_along(adm_list$reg_tz), function(x){
    sim_by[[y]][[adm_list$reg_tz[x]]]$stats$stats_sum},
    .id = "reg_tz"
  ), .id = "ssp_y"
)
table(sim_by_stats$converged)


# ========================================================================================
# NOT BY SIMULATION ----------------------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# We exclude any families that only consist of m15 members. If not, ipu cannot solved as
# there is no matching data in per seed. In this case n = 1
hh_seed_no_by <- hh_db %>%
  filter(seed_hh_size != seed_hh_size_p15) %>%
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
sim_no_by <- map(ssp_y$ssp_y, reweigh, adm_list$reg_tz[3], hh_seed_no_by, per_seed_no_by, bm_no_by, param,
                 verbose = FALSE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
                 absolute_diff = 10, parallel = TRUE, output = temp_path)
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


# ========================================================================================
# COMBINE SIMULATIONS --------------------------------------------------------------------
# ========================================================================================

# TO DO: Convert all sims to data.frames with columns that include scenario and year and bind.
# In this way, all simulations can quickly be filtered, etc.


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

temp_path <- file.path(param$model_path, glue("simulation/{Sys.Date()}"))
dir.create(temp_path, recursive = T, showWarnings = F)
saveRDS(sim_by, file.path(temp_path, glue("ssp1_3_by_{param$iso3c}.rds")))
saveRDS(sim_no_by, file.path(temp_path, glue("ssp1_3_no_by_{param$iso3c}.rds")))


# ========================================================================================
# PLOT WEIGHTS ---------------------------------------------------------------------------
# ========================================================================================

# UPDATE: Create function that compares across years per region?
# Create pdf to inspect weights of histogram
pdf(file = file.path(temp_path, "ssp1_3_by_weights.pdf")) # , width = 8.27, height = 11.69
walk(adm_list$reg_tz, create_weights_plot, sim_file = sim_by[[1]])
dev.off()

pdf(file = file.path(temp_path, "ssp1_3_2050_weights.pdf")) # , width = 8.27, height = 11.69
walk(adm_list$reg_tz, create_weights_plot, sim_file = sim_no_by[[1]])
dev.off()

