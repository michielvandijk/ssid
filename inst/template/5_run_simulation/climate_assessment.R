# ========================================================================================
# Project:  sidd
# Subject:  Script to overlay results with heat stress map
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# HEAT STRESS CALCULATION ----------------------------------------------------------------
# ========================================================================================

db_path <- "C:/Users/dijk158/OneDrive - Wageningen University & Research/data/microsim_db"
temp_path <- file.path(db_path, "climate_impact/schwingshackl_et_al_2021")

# Load file
library(terra)
f <- rast(file.path(temp_path, "WBGTs-ThrExc_Level3_HadGEM3-GC31-LL_historical-ssp585_1991-2090.nc"))
r <- rast(f)
r <- crop(f, vect(adm), snap = "out")

# Map
# 26h map is 2016
r_map <- r[[c(26,60)]]
names(r_map) <- c("2016", "2050")
r_map_df <- as.data.frame(r_map, xy = TRUE) %>%
  pivot_longer(-c(x, y), names_to = "year", values_to = "share")

r_map_df %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = share)) +
  geom_sf(data = adm, fill = "transparent") +
  scale_fill_viridis_c(direction = -1) +
  labs(x = NULL, y = NULL, fill = "Number of days with WBGs > 32 degrees") +
  coord_sf() +
  theme_void() +
  facet_wrap(~year) +
  #theme(strip.text = element_text(hjust = 0)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Create cut of maps
r[r < 36.5] <- 0
r[r >= 36.5] <- 1

# Function to calculate weighted share
# Note that we are using SSP5 extreme scenario!!
calc_share <- function(t, r){
  r <- r[[t]]
  y <- lubridate::year(time(r))
  df <- adm %>%
    st_drop_geometry() %>%
    mutate(
      share = exact_extract(r, adm, fun = "mean"),
      year = y,
      scenario = "SSP3"
    )
  return(df)

}



# ========================================================================================
# COMBINE WITH INCOME AND POVERTY PROJECTIONS -------------------------------------------------------
# ========================================================================================

adm_df_cc <- adm %>%
  left_join(hci_adm2 %>%
              filter(urban_rural == "total")) %>%
  mutate(headcount2 = cut(headcount, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), include.lowest = TRUE),
         scenario = toupper(scenario))


# Income
adm_df_cc %>%
  filter(year %in% c(2016, 2050), scenario == "SSP3") %>%
  mutate(poor = round(poor/1000000)) %>%
  ggplot() +
  geom_sf(aes(fill = poor)) +
  facet_wrap(vars(factor(year))) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(direction = -1, option = "inferno") +
  guides(fill = guide_coloursteps(reverse = FALSE, show.limits = TRUE)) +
  labs(fill = "Number of the poor (million)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

library(exactextractr)
# Combine with poverty
hs_df <-adm_df_cc %>%
  left_join(map_df(c(26, 60), calc_share, r = r)) %>%
  filter(!is.na(share)) %>%
  mutate(poor_hs = poor * share/1000000)

ggplot(data = hs_df) +
  geom_sf(aes(fill = poor_hs)) +
  facet_wrap(vars(factor(year))) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
  guides(fill = guide_coloursteps(reverse = FALSE, show.limits = FALSE)) +
  labs(fill = "Number of the poor (million)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

