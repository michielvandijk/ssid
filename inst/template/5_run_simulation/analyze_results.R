# ========================================================================================
# Project:  sidd
# Subject:  Script to analyze spatial simulations
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load additional packages
p_load(ggridges, ggpubr)


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here("working_paper/scripts/model_setup/set_model_parameters.r"))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# simulation
version <- "2022-09-16"
sim_db <- readRDS(file.path(param$model_path, glue("simulation/{version}/sim_db_{param$iso3c}.rds")))

# WDI data
wdi_raw <- readRDS(file.path(param$db_path, glue("wdi/wdi.rds")))


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================


# PREPARE POVERTY LINE -------------------------------------------------------------------

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
pl <- pl_db$pl[pl_db$year == param$base_year]
pl


# ========================================================================================
# NATIONAL LEVEL RESULTS -----------------------------------------------------------------
# ========================================================================================

# DISTRIBUTION ---------------------------------------------------------------------------
# Manually excluded outliers
# Add figure that shows relative/absolute size of urban/rural over time.
# Add histogram?
# Check: https://stackoverflow.com/questions/68395187/add-a-y-axis-with-density-to-ridgeline-plot
# CHECK calculation. Does not seem correct as area under the density plot should be one in both cases.
# Is unlikely as one is much bigger than the other.
fig_dist_adm0 <- sim_db %>%
  ggplot(aes(
    x = (per_income_proj), y = factor(year), height = stat(density),
    weight = per_weight, fill = stat(x)
  )) +
  geom_density_ridges_gradient(
    #jittered_points = TRUE,
    scale = 2,
    stat = "density",
    rel_min_height = 0.02,
    panel_scaling = FALSE # ensures scaling is comparable between facets
  ) +
  facet_wrap(~ scenario) +
  geom_vline(xintercept = (pl), linetype = "dashed") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35000), label = comma) +
  coord_cartesian(clip = "off") +
  scale_fill_viridis_c(name = "national currency", option = "C") +
  guides(fill = "none") +
  labs(y = NULL, x = "Birr") +
  theme(legend.position = "bottom")
fig_dist_adm0


# HEADCOUNT INDEX ------------------------------------------------------------------------
hci_nat <- bind_rows(
  sim_db %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(
      headcount = poor / tot * 100,
      urban_rural = "total"
    ),
  sim_db %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, urban_rural) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(headcount = poor / tot * 100)
)

fig_hci_adm0 <- hci_nat %>%
  mutate(urban_rural = factor(urban_rural, level = c("rural", "urban", "total"))) %>%
  filter(urban_rural == "total") %>%
  ggplot(aes(x = factor(year), y = headcount, fill = urban_rural)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  facet_wrap(~scenario) +
  labs(x = NULL, y = "poverty headcount (%)") +
  guides(fill = "none")
fig_hci_adm0

fig_hci_adm0_ur <- hci_nat %>%
  mutate(urban_rural = factor(urban_rural, level = c("rural", "urban", "total")),
         poor = poor/1000000) %>%
  filter(urban_rural != "total") %>%
  ggplot(aes(x = factor(year), y = poor, fill = urban_rural)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  facet_wrap(~scenario) +
  labs(x = NULL, y = "Poor population (million)", fill = NULL, title = "Poor population") +
  #guides(fill = "none") +
  theme_bw()
fig_hci_adm0_ur


# ========================================================================================
# ADM1 -----------------------------------------------------------------------------------
# ========================================================================================


# adm1_level
hci_adm1 <- bind_rows(
  sim_db %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, adm1_name) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(
      headcount = poor / tot * 100,
      urban_rural = "total"
    ),
  sim_db %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, adm1_name, urban_rural) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(headcount = poor / tot * 100)
)


fig_hc_adm1 <- hci_adm1 %>%
  filter(urban_rural != "total") %>%
  mutate(poor = poor/1000000) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = poor, fill = urban_rural)) +
  facet_grid(~scenario)
fig_hc_adm1


# ========================================================================================
# ADM2 ------------------------------------------------------------------------------------
# ========================================================================================


# adm2_level
hci_adm2 <- bind_rows(
  sim_db %>%
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
  sim_db %>%
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
  mutate(headcount2 = cut(headcount, breaks = c(0,10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
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

# URBAN-RURAL ----------------------------------------------------------------------
hci_adm2_u_r <- bind_rows(
  sim_db %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, adm1_name, adm2_code, urban_rural) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(
      headcount = poor / tot * 100,
      urban_rural = "total"
    ),
  sim_db %>%
    mutate(poor = per_income_proj < pl) %>%
    group_by(year, scenario, adm1_name, adm2_code, urban_rural) %>%
    summarize(
      tot = sum(per_weight),
      poor = sum(per_weight[poor]), .groups = "drop"
    ) %>%
    mutate(headcount = poor / tot * 100)
)

adm_s <- st_simplify(adm, dTolerance = 5000)

adm_df <- adm_s %>%
  left_join(hci_adm2 %>%
              filter(urban_rural != "total")) %>%
  mutate(headcount2 = cut(headcount, breaks = c(0,10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
         scenario = toupper(scenario)) %>%
  filter(year %in% c(2018, 2050))

ggplot(data = filter(adm_df, urban_rural == "urban")) +
  geom_sf(aes(fill = headcount2)) +
  #facet_grid(vars(scenario), vars(factor(year))) +
  facet_grid(vars(scenario), vars(factor(year))) +
  theme(legend.position = "bottom") +
  #scale_fill_distiller(palette = "YlOrBr") +
  # scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = 20) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  guides(fill = guide_coloursteps(reverse = FALSE, show.limits = TRUE)) +
  labs(fill = "Poverty headcount (%)", title = "Urban population") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

ggplot(data = filter(adm_df, urban_rural == "rural")) +
  geom_sf(aes(fill = headcount2)) +
  #facet_grid(vars(scenario), vars(factor(year))) +
  facet_grid(vars(scenario), vars(factor(year))) +
  theme(legend.position = "bottom") +
  #scale_fill_distiller(palette = "YlOrBr") +
  # scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = 20) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  guides(fill = guide_coloursteps(reverse = FALSE, show.limits = TRUE)) +
  labs(fill = "Poverty headcount (%)", title = "Rural population") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


# CONVERGENCE ANALYSIS ------------------------------------------------------------

# hci_adm2 %>%
#   ggplot(aes(x = year, y = headcount, group = adm2_code)) +
#   geom_line() +
#   facet_grid(~urban_rural)



# ========================================================================================
# COMPARE IMPACT OF DRIVERS --------------------------------------------------------------
# ========================================================================================


# Baseline
# Demographical change (sex, age)
# Urban-rural
# Change in m15
# Structural change (occ, including participation rate = not_in_lf share)
# Income increase induced by productivity growth

# TO CHECK. Why is impact urbanization so small?

driver_impact <- bind_rows(
  sim_db %>%
    filter(year == param$base_year) %>%
    mutate(
      per_income_comp = per_income_proj,
      driver = as.character(param$base_year)
    ),
  sim_db %>%
    filter(year == 2050) %>%
    mutate(
      per_income_comp = hh_income / hh_size,
      driver = "occ, urban_rural, age, sex"
    ),
  sim_db %>%
    filter(year == 2050) %>%
    mutate(
      per_income_comp = hh_income / hh_size_s,
      driver = "2050 - Demograhic change and urbanization"
    ),
  sim_db %>%
    filter(year == 2050) %>%
    mutate(
      per_income_comp = per_income_proj,
      driver = "2050 - Demograhic change, urbanization and income change"
    )
)

med_pc_income <- driver_impact %>%
  group_by(driver, year, scenario) %>%
  summarize(median = median((per_income_comp)))

fig_driver <- driver_impact %>%
  filter(driver != "occ, urban_rural, age, sex") %>%
  ggplot() +
  geom_density(
    aes(x = (per_income_comp), weight = per_weight, color = driver),
    size = 1
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 45000), label = comma) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "horizontal") +
  labs(x = "Birr", color = NULL) +
  facet_grid(~scenario)
fig_driver


# Impact of urbanization and occ change seems small
# CHECK projections for urbanization and occ change. Perhaps drivers are small.

