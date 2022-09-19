# ========================================================================================
# Project:  siss
# Subject:  Script to simulate income change
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

# Estimated income
est_income <- readRDS(file.path(param$model_path, glue("simulation/estimated_income_{param$iso3c}.rds")))

# hh_db
hh_db <- readRDS(file.path(param$model_path, glue("simulation/hh_db_{param$iso3c}.rds")))

# per_db
per_db <- readRDS(file.path(param$model_path, glue("simulation/per_db_{param$iso3c}.rds")))

# Spatial simulation
version <- "2022-09-16"
sim_by <- readRDS(file.path(param$model_path, glue("simulation/{version}/ssp1_3_by_{param$iso3c}.rds")))
sim_no_by <- readRDS(file.path(param$model_path, glue("simulation/{version}/ssp1_3_no_by_{param$iso3c}.rds")))

# Adm_list
adm_list <- readRDS(file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))

# Subnat population projections
subnat_age_sex_proj <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_age_sex_proj_{param$iso3c}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_age_sex_proj_m15_{param$iso3c}.rds")))

# MAGNET wage and price projections
wage_proj_raw <- readRDS(file.path(param$model_path, glue("simulation/wage_proj_{param$iso3c}.rds")))
price_proj_raw <- readRDS(file.path(param$model_path, glue("simulation/price_proj_{param$iso3c}.rds")))

# WDI data
wdi_raw <- readRDS(file.path(param$db_path, glue("wdi/wdi.rds")))


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================

sim_db <- bind_rows(
  map_df(sim_by, ~map_df(.x, "weight_tbl", .id = "name"), .id = "name2"),
  map_df(sim_no_by, ~map_df(.x, "weight_tbl", .id = "name"), .id = "name2")
  ) %>%
    separate(name, into = c("adm1_name", "adm2_code"), sep = "-x-") %>%
    separate(name2, into = c("scenario", "year"), sep = "_") %>%
  mutate(source = "simulation",
         year = as.integer(year))

# Remove raw data to save memory space
#rm(sim_by, sim_no_by)

# CHECK THIS CODE -------------------------------------------------------------------------
# We link the hh_db ONLY on hh_id because we are using the full survey in each adm2 simulation.
# This means in the sim_db the same hh_id can occur in multiple adm1 and adm2.
# If the survey is bigger and we use the adm1 part of the survey as seed for all underlying
# adm2, we can also join on adm1 (but this is perhaps not necessary)

# sim_db <- sim_db %>%
#   dplyr::select(adm1_name, adm2_code, hh_id = id, hh_weight = weight, source, year, scenario) %>%
#   left_join(hh_db)

sim_db <- sim_db %>%
  dplyr::select(adm1_name, adm2_code, hh_id = id, hh_weight = weight, source, year, scenario) %>%
  left_join(hh_db %>%
              select(hh_id, ea_id, urban_rural, survey_hh_weight, hh_size, hh_n_m15))

# Remove NAs because sim_db was run with inconsistent seed that includes hh with NA income info
summary(sim_db)
sim_db <- sim_db %>%
  na.omit()

# select relevant years for wage proj
wage_proj <- wage_proj_raw %>%
  filter(year %in% c(param$base_year, 2020, 2030, 2040, 2050))

# Prepare price projections
food_commodities <- c("pdr", "wht", "gro", "veg", "fruit", "nuts", "roots", "pulses", "osd",
                      "c_b", "fsh", "aqcltr", "pltry", "pig", "bfctl", "ctl", "rmk", "pcr",
                      "vegoil", "sgr", "fishp", "pltrymt", "pork", "beef", "rummt", "dairy")

price_proj <- price_proj_raw %>%
  filter(commodity %in% food_commodities) %>%
  group_by(region, scenario, year) %>%
  summarize(price_index = weighted.mean(price_cons_good_market_price, vpb_volume),
            .groups = "drop") %>%
  mutate(year = as.integer(year))


# ========================================================================================
# CHECK SUM OF WEIGHTS BETWEEN SEED AND SIMULATION----------------------------------------
# ========================================================================================

# Note that the base year for the simulation is often different and more recent than the
# survey so differences are expected.

# Households
sum(hh_db$survey_hh_weight)
sim_db %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(hh_weight))

# Population
# CHECK THIS CALCULATION. questionable if hh_size can be uses to estimate total population in
# SIM as this will also change (become smaller) because of demographic change.
sum(hh_db$survey_hh_weight*hh_db$hh_size)
sim_db %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(hh_weight*hh_size, na.rm = TRUE))


# ========================================================================================
# PROJECT INCOME--------------------------------------------------------------------------
# ========================================================================================

# CALCULATE NUMBER OF CHILDREN -----------------------------------------------------------

# Determine actual m15 by adm2 from projections
# Calculate projected number of children using base weights
# Recalculate number of children by scaling original hh_n_m15.
# This implies number of children are not an integer anymore. This is not a problem as
# all hh have a weight so on average the number of children can be fractional, in reality
# the underlying distribution can be different integer number of children per hh.

# TO UPDATE. We also calculate scaling factors for the base year which should be close to 1 if ipfr
# would converge fully (and hh are also selected on hh_size?) => Check how hh_size x survey_weights relates
# to total population. If this is not approximately the same hh_size does not correspond well with weight.
# For some adm2 this is not the case => check convergence statistics.

# m15 scaling factor for projections
m15_sf <- bind_rows(
  subnat_age_sex_proj %>%
    filter(scenario %in% unique(sim_db$scenario), year %in% unique(sim_db$year)) %>%
    filter(age == "m15") %>%
    mutate(source = "subnat_proj") %>%
    group_by(source, year, scenario, adm1_name, adm2_code) %>%
    summarize(m15 = sum(value, na.rm = TRUE), .groups = "drop"),
  sim_db %>%
    group_by(source, year, scenario, adm1_name, adm2_code) %>%
    summarize(m15 = sum(hh_weight*hh_n_m15), .groups = "drop")) %>%
  group_by(adm1_name, adm2_code, year, scenario) %>%
  summarize(sf = m15[source == "subnat_proj"]/m15[source == "simulation"],
            .groups = "drop")

sim_db <- sim_db %>%
  left_join(m15_sf) %>%
  mutate(hh_n_m15_s = hh_n_m15 * sf,
         hh_size_s = hh_size + hh_n_m15_s - hh_n_m15)


# SIMULATE HH INCOME ---------------------------------------------------------------------

# Calculate hh income by combining income projections and per income
# Filter out m15 as income is 0 by definition
sim_hh_income <- per_db %>%
  left_join(est_income$per_income) %>%
  filter(occupation != "m15") %>%
  left_join(wage_proj) %>%
  group_by(hh_id, year, scenario) %>%
  summarize(occ_hh_income = sum(per_income_est*occ_wage_proj, na.rm = TRUE),
            .groups = "drop")

# Update residual proportional to increase in income_occ if residual is not zero
sim_hh_income <- left_join(est_income$hh_income, sim_hh_income) %>%
  mutate(occ_hh_income_growth = ifelse(hh_income_ex_resid != 0, occ_hh_income/hh_income_ex_resid, 0),
         residual_update = occ_hh_income_growth*residual,
         hh_income_proj = occ_hh_income + residual_update,
         hh_income_growth = hh_income_proj/hh_income)


# SIMULATE PC_INCOME ----------------------------------------------------------------------

# Calculate per capita income by dividing proj hh income by household size
# Calculate person level weight by multiplying hh weight with household size.
sim_db <- sim_db %>%
  left_join(sim_hh_income) %>%
  mutate(per_income_proj = hh_income_proj/hh_size_s,
         per_weight = hh_weight*hh_size_s)

############## CHECK DIFFERENCE IN TOTALS. MUST BE IN HH_SIZE_S vs HH_SIZE
# CHECK how to treat not_in_lf. Now we estimate wage and add but perhaps set to zero contribution,
# simular to m15.
sim_db %>%
  group_by(year, scenario) %>%
  summarize(pop = sum(hh_weight*hh_size, na.rm = TRUE),
            pop2 = sum(per_weight, na.rm = TRUE))
summary(sim_db$per_weight)


# ========================================================================================
# SIMULATE POVERTY -----------------------------------------------------------------------
# ========================================================================================

# CALCULATE POVERTY LINE -----------------------------------------------------------------
# Select pl base year for the year official pl estimate is available for comparison
ipl <- 1.9
pl_db <- wdi_raw %>%
  filter(adm0_code == param$iso3c) %>%
  mutate(across(everything(), zap_label)) %>%
  filter(adm0_code == param$iso3c) %>%
  dplyr::select(year, FP.CPI.TOTL, PA.NUS.PRVT.PP) %>%
  mutate(
    cpi_2011 = FP.CPI.TOTL / FP.CPI.TOTL[year == 2011],
    ppp_2011 = PA.NUS.PRVT.PP[year == 2011],
    pl = ipl * ppp_2011 * cpi_2011 * 365
  ) %>%
  filter(year %in% c(2011:param$base_year))
pl_base <- pl_db$pl[pl_db$year == param$base_year]
pl_base

# Interpolate and create price index with base_year = 1
price_proj <- price_proj %>%
  complete(scenario, region, year = c(2014:2050)) %>%
  mutate(price_index = na_interpolation(price_index)) %>%
  group_by(scenario) %>%
  mutate(price_index = price_index/price_index[year == param$base_year]) %>%
  ungroup() %>%
  filter(year >= param$base_year)

# Poverty line taking into account increase in (food prices)
# For the pilot illustration we increase the SSP2 and SSP3 prices artificially
price_proj_upd <- price_proj
price_proj_upd$price_index[price_proj_upd$scenario == "ssp3"] <- seq(1, 3, length.out = 35)
price_proj_upd$price_index[price_proj_upd$scenario == "ssp2"] <- seq(1, 3, length.out = 35)
ggplot(data = price_proj) +
  geom_line(aes(y = price_index, x = year, color = scenario))


ggplot(data = price_proj_upd) +
  geom_line(aes(y = price_index, x = year, color = scenario))
pl_scenarios <- price_proj_upd %>%
  mutate(pl = price_index * pl_base)

# Add to sim_db
sim_db_pov <- sim_db %>%
  left_join(pl_scenarios)

# POVERTY ----------------------------------------------------------------------------------

hci_adm2 <- bind_rows(
  sim_db_pov %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, adm1_name, adm2_code) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(
      headcount = poor / tot * 100,
      urban_rural = "total"
    ),
  sim_db_pov %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, adm1_name, adm2_code, urban_rural) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(headcount = poor / tot * 100)
)

adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))
adm_s <- st_simplify(adm, dTolerance = 5000)

adm_df <- adm_s %>%
  left_join(hci_adm2 %>%
              filter(urban_rural == "total")) %>%
  mutate(headcount2 = cut(headcount, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), include.lowest = TRUE),
         scenario = toupper(scenario))

fig_hci_adm2 <- ggplot(data = adm_df) +
  geom_sf(aes(fill = headcount2)) +
  #facet_grid(vars(scenario), vars(factor(year))) +
  facet_grid(vars(factor(year)), vars(scenario)) +
  theme(legend.position = "bottom") +
  #scale_fill_distiller(palette = "YlOrBr") +
  # scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = 20) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  guides(fill = guide_coloursteps(reverse = FALSE, show.limits = TRUE)) +
  labs(fill = "Poverty headcount (%)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
fig_hci_adm2


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(sim_db, file.path(param$model_path, glue("simulation/{version}/sim_db_{param$iso3c}.rds")))
