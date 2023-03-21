# ========================================================================================
# Project:  simFNS
# Subject:  Script to analyze spatial simulations
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue)

# Load additional packages
p_load(haven, sf, ggridges, ggpubr)

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# simulation
version <- "2022-05-31"
sim_db <- readRDS(file.path(proc_path, glue("simulation/{version}/sim_db_{iso3c_sel}.rds"))) 

# WDI data
wdi_raw <- readRDS(file.path(raw_path, glue("wdi/wdi.rds")))


# ========================================================================================
# PREPARE --------------------------------------------------------------------------------
# ========================================================================================


# PREPARE POVERTY LINE -------------------------------------------------------------------
ipl <- 1.9
pl_db <- wdi_raw %>%
  filter(adm0_code == "ETH") %>%
  mutate(across(everything(), zap_label)) %>%
  filter(adm0_code == iso3c_sel) %>%
  dplyr::select(year, FP.CPI.TOTL, PA.NUS.PRVT.PP) %>%
  mutate(
    cpi_2015 = FP.CPI.TOTL / FP.CPI.TOTL[year == 2015],
    ppp_2015 = PA.NUS.PRVT.PP[year == 2015],
    pl = ipl * ppp_2015 * cpi_2015 * 365
  ) %>%
  filter(year %in% c(2010:2018))
pl <- pl_db$pl[pl_db$year == 2018]

# 2015 pl from 2020 poverty assessment updated to 2018
#pl <- 7184*pl_db$cpi_2015[pl_db$year == 2018] 

# SIMPLIFY MAP ---------------------------------------------------------------------------
adm <- readRDS(file.path(proc_path, glue("adm/adm2_{iso3c_sel}.rds")))
adm_s <- st_simplify(adm, dTolerance = 5000)


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


adm_df <- adm_s %>%
  left_join(hci_adm2) %>%
  mutate(headcount2 = cut(headcount, breaks = c(0,20, 40, 60, 80, 100)))

fig_hci_adm2 <- ggplot(data = adm_df) +
  geom_sf(aes(fill = headcount2)) +
  facet_grid(vars(scenario), vars(factor(year))) +
  theme(legend.position = "bottom") +
  #scale_fill_distiller(palette = "YlOrBr") +
  # scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = 20) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  guides(fill = guide_coloursteps()) +
  labs(fill = "Poverty headcount (%)")


# ========================================================================================
# ANIMATION ------------------------------------------------------------------------------
# ========================================================================================

col_cb   <- c("#202020", "#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
show_col(col_cb)

# Function to calculate density
ds <- function(x) {
  dens <- density((x$per_income_proj), weight = x$weight, n = 4096) # High n to create smoother density that reduces white space between segments
  df <- data.frame(x=dens$x, y=dens$y)
  df$hc <- round(unique(x$headcount),0)
  df$hc <- glue("Poverty\nheadcount {df$hc} (%)")
  return(df)
}

# Estimate weighted density for each year
# Link hc at adm2 to sim_db
dens <- sim_db %>%
  left_join(hci_adm2 %>%
              filter(urban_rural == "total") %>%
              dplyr::select(year, scenario, adm2_code, headcount)) %>%
  filter(adm2_code == "231004006") %>% #231004006
  group_by(year, scenario) %>%
  mutate(weight = per_weight/sum(per_weight, na.rm = TRUE),
         id = paste(year, scenario, sep = "_")) %>%
  ungroup() %>%
  split(list(.$id)) %>%
  map_dfr(ds, .id = "id") %>%
  separate(id, into = c("year", "scenario"), sep = "_") %>%
  mutate(scenario = toupper(scenario))

# Create poverty headcount labels
lab <- dens %>%
  group_by(year, scenario) %>%
  summarize(hc = unique(hc))


# Plot
library(gganimate)
anim <- ggplot(data = dens, aes(x,y, label = hc)) +
  geom_text(data = lab, aes(label = hc, x = 40000, y = 0.000050), size = 5) +
  geom_ribbon(data = filter(dens, x <= (pl)), aes(x=x, ymax = y), ymin=0, fill = col_cb[3]) + 
  geom_ribbon(data = filter(dens, x >= (pl)), aes(x=x, ymax = y), ymin=0, fill = col_cb[4]) +
  facet_grid(~scenario) +
  geom_vline(xintercept = (pl), linetype = "dashed", size = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 70000), label = comma) +
  labs(x = "Income per capita (2018 Birr)", y = "density") +
  annotate("text", x = 40000, y = 0.000095, label = "National poverty line", size = 5) +
  transition_states(factor(year),
                    transition_length = 2,
                    state_length = 1) +
  ease_aes('cubic-in-out') +
  ggtitle('Year: {closest_state}') +
  #theme_bw() +
  rremove("grid") +
  theme(plot.title = element_text(hjust = 0.5, size = 22))

animate(anim, fps = 20, height = 400, width = 800)  


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

temp_path <- file.path(proc_path, "scratch")
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
anim_save(file.path(temp_path, "ssp1_3_district_animation.gif"), anim, 
          fps = 20, height = 400, width = 600)
