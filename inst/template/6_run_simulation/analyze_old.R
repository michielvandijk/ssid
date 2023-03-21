# ========================================================================================
# Project:  ssid
# Subject:  Script to analyze spatial simulations
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# Load additional packages
p_load(haven, sf, ggridges, ggpubr)

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



# ========================================================================================
# SDG 1 ASSESSMENT -----------------------------------------------------------------------
# ========================================================================================

# PREPARE POVERTY LINE -------------------------------------------------------------------

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
  filter(year %in% c(2010:param$base_year))
pl <- pl_db$pl[pl_db$year == param$base_year]



# HEADCOUNT INDEX ------------------------------------------------------------------------

# National level
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

# ========================================================================================
# DECOMPOSITION -------------------------------------------------------------------------
# ========================================================================================

# adm2 contribution to headcount index
hci_share_adm2 <- sim_db %>%
  mutate(poor = per_income_proj < pl) %>%
  filter(poor) %>%
  group_by(year, scenario, adm1_name, adm2_code) %>%
  summarize(poor = sum(per_weight), .groups = "drop") %>%
  group_by(year, scenario, .groups = "drop") %>%
  mutate(share = poor / sum(poor, na.rm = T))

ggplot(data = hci_share_adm2) +
  geom_col(aes(x = factor(year), y = poor, fill = adm1_name)) +
  facet_grid(~scenario)

# P 166 handbook on how to decompose change in headcount into redistribution and growth effect.
# Shift share proposed by Huppi and Ravalion (1991) to show contribution of sectors or regions.
# Decomposition used by Stefest that decomposes by means of simulating additions.
#' run model each time updating driver to t+1:
#' - age
#' - sex
#' - urban-rural
#' - occupation
#' - income
#' - household size
#'
#'


# ========================================================================================
# VISUALIZE ------------------------------------------------------------------------------
# ========================================================================================

order_adm2_code <- hci_adm2 %>%
  filter(urban_rural == "total", year == 2050) %>%
  arrange(headcount)

# Ranking
# UPDATE add uncertainty interval
# Need to work on labels..
# https://uc-r.github.io/cleveland-dot-plots
right_label <- hci_adm2 %>%
  group_by(adm2_code, year, urban_rural, scenario) %>%
  arrange(desc(headcount)) %>%
  top_n(1)

left_label <- hci_adm2 %>%
  group_by(adm2_code, year, urban_rural, scenario) %>%
  arrange(desc(headcount)) %>%
  slice(2)

hci_adm2 %>%
  mutate(
    urban_rural = factor(urban_rural, levels = c("total", "urban", "rural")),
    adm2_code = factor(adm2_code, levels = unique(order_adm2_code$adm2_code))
  ) %>%
  ggplot(aes(x = adm2_code, y = headcount, color = factor(year))) +
  geom_point() +
  geom_line(aes(group = adm2_code), color = "black", linetype = "dashed") +
  # geom_text(data = right_label, aes(color = factor(year), label = round(headcount, 0)),
  #           size = 3, hjust = -.5) +
  # geom_text(data = left_label, aes(color = factor(year), label = round(headcount, 0)),
  #           size = 3, hjust = 1.5) +
  coord_flip() +
  facet_wrap(scenario~urban_rural) +
  theme_bw() +
  theme(legend.position = "bottom")

# Map
# Adm

library(sf)
adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))

adm_s <- st_simplify(adm, dTolerance = 5000)
plot(adm_s$geometry)

adm_df <- adm_s %>%
  left_join(hci_adm2) %>%
  mutate(headcount2 = cut(headcount, breaks = c(0,20, 40, 60, 80, 100)))

ggplot(data = adm_df) +
  geom_sf(aes(fill = headcount2)) +
  facet_grid(vars(scenario), vars(factor(year))) +
  theme(legend.position = "bottom") +
  # scale_fill_distiller(palette = "YlOrBr") +
  # scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = 20) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_coloursteps())


# https://github.com/wilkelab/ggridges/issues/5#issuecomment-338444835
# To add weight to density function
ggplot(data = sim_db, aes(
  x = log(per_income_proj), y = factor(year), height = stat(density),
  weight = per_weight, fill = stat(x)
  )) +
  geom_density_ridges_gradient(
    scale = 2,
    stat = "density",
    rel_min_height = 0.02,
  ) +
  facet_wrap(scenario~ factor(year)) +
  geom_vline(xintercept = log(pl), linetype = "dashed") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  theme(legend.position = "bottom")


ggplot(data = sim_db, aes(
  x = log(per_income_proj), y = adm1_name, height = stat(density),
  weight = per_weight)) +
  geom_density_ridges_gradient(
    scale = 2,
    stat = "density",
    rel_min_height = 0.02,
  ) +
  facet_grid(urban_rural ~ factor(year)) +
  geom_vline(xintercept = log(pl), linetype = "dashed") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  theme(legend.position = "bottom")




# COMPARE IMPACT OF DRIVERS --------------------------------------------------------------

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
      driver = "initial"
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
      driver = "occ, urban_rural, age, sex, m15"
    ),
  sim_db %>%
    filter(year == 2050) %>%
    mutate(
      per_income_comp = per_income_proj,
      driver = "MAGNET"
    )
)

med_pc_income <- driver_impact %>%
  group_by(driver, year, adm2_code) %>%
  summarize(median = median(log(per_income_comp)))

ggplot() +
  geom_density(
    data = driver_impact, aes(x = log(per_income_comp), weight = per_weight, color = driver),
    alpha = 0.4
  ) +
  geom_vline(
    data = med_pc_income, aes(
      xintercept = median,
      color = driver
    ),
    size = 1, linetype = "dashed"
  ) +
  theme_bw() +
  facet_wrap(~adm2_code)

# Impact of urbanization and occ change seems small
# CHECK projections for urbanization and occ change. Perhaps drivers are small.

adm_sel <- "DHAKA"

ggplot() +
  geom_density(
    data = filter(sim_db, adm1_name == adm_sel), aes(
      x = log(pc_income), weight = per_weight,
      color = interaction(year, scenario)
    ),
    alpha = 0.4
  ) +
  geom_vline(
    data = pl, aes(xintercept = log(value)),
    size = 1, linetype = "dashed"
  ) +
  theme_bw() +
  facet_wrap(~adm2_code)

ggplot(data = sim_df, aes(log(pc_income), color = interaction(year, scenario))) +
  stat_ecdf(geom = "step")






# ANIMATION ------------------------------------------------------------------------------

col_cb   <- c("#202020", "#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
show_col(col_cb)

# Function to calculate density
ds <- function(x) {
  dens <- density((x$per_income_proj), weight = x$weight)
  df <- data.frame(x=dens$x, y=dens$y)
  df$hc <- unique(x$headcount)
  df$hc <- glue("Poverty headcount {df$hc} (%)")
  return(df)
}

# Estimate weighted density for each year
# Link hc at adm2 to sim_db
dens <- sim_db %>%
  left_join(hci_adm2 %>%
              filter(urban_rural == "total") %>%
              dplyr::select(year, scenario, adm2_code, headcount)) %>%
  filter(adm2_code == "231001004") %>%
  group_by(year, scenario) %>%
  mutate(weight = per_weight/sum(per_weight, na.rm = TRUE),
         id = paste(year, scenario, sep = "_")) %>%
  ungroup() %>%
  split(list(.$id)) %>%
  #split(list(.$scenario, .$year)) %>%
  map_dfr(ds, .id = "id") %>%
  separate(id, into = c("year", "scenario"), sep = "_")

# Create poverty headcount labels
lab <- dens %>%
  group_by(year, scenario) %>%
  summarize(hc = unique(hc))


# Plot
library(gganimate)
anim <- ggplot(data = dens, aes(x,y, label = hc)) +
  #geom_text(data = lab, aes(label = hc, x = 8.8, y = 0.65)) +
  geom_ribbon(data = filter(dens, x < (pl)), aes(x=x, ymax = y), ymin=0, fill = col_cb[3]) +
  geom_ribbon(data = filter(dens, x > (pl)), aes(x=x, ymax = y), ymin=0, fill = col_cb[4]) +
  facet_grid(~scenario) +
  geom_vline(xintercept = (pl), linetype = "dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 70000)) +
  labs(x = "Income per capita (2018 Taka)", y = "density") +
  #annotate("text", x = 9, y = 0.8, label = "National poverty line") +
  transition_states(factor(year),
                    transition_length = 2,
                    state_length = 1) +
  ease_aes('cubic-in-out') +
  ggtitle('Year: {closest_state}') +
  #theme_bw() +
  rremove("grid")

animate(anim, fps = 20, height = 400, width = 600)
anim_save(file.path(param$model_path, glue("simulation/{version}/ssp1_3_animation_district.gif")), anim,
          fps = 20, height = 400, width = 600)
