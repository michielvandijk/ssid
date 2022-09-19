# ========================================================================================
# Project:  ssid
# Subject:  Script to extract MAGNET results
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here("working_paper/scripts/model_setup/set_model_parameters.r"))

# remotes::install_github("Thijs-de-Lange/magnetr")
library(magnetr)


# ========================================================================================
# MAGNET PARAMETERS ----------------------------------------------------------------------
# ========================================================================================

iso3c_magnet <- "bgd"
magnet_path <- "W:/WECR/Magnet_data4/Thijs/Dhaka_uganda_5lab/4_MAGNET"
dataBasePath <- file.path(magnet_path,'Basedata')
dataUpdatesPath <- file.path(magnet_path,'Updates')
dataSolutionPath <- file.path(magnet_path,'Solutions')

file_path_shocks <- file.path(magnet_path,'Shocks')
shocks <- c("Shocks_ssp1_5lab_labshock", "Shocks_ssp2_5lab_labshock", "Shocks_ssp3_5lab_labshock")

country <- c("BGD")
scenario <-c("BaseGDPExo_msx_SSP1_5lab_labshock", "BaseGDPExo_msx_SSP2_5lab_labshock", "BaseGDPExo_msx_SSP3_5lab_labshock")
period <- c("2014-2018", "2018-2020", "2020-2025", "2025-2030", "2030-2035" ,"2035-2040", "2040-2045", "2045-2050")
base_year <- "2014"


file_type <- "update"
file_suffix <- "har"


# ========================================================================================
# EXTRACT DATA ---------------------------------------------------------------------------
# ========================================================================================

# POPULATION -----------------------------------------------------------------------------
pop <- magnetr::magnet_indicator("pop", scenario, period, base_year, magnet_path) %>%
  filter(region == iso3c_magnet) %>%
  rename(pop = value) %>%
  mutate(
    scenario = case_when(
      scenario ==  "BaseGDPExo_msx_SSP1_5lab_labshock" ~ "SSP1",
      scenario ==  "BaseGDPExo_msx_SSP2_5lab_labshock" ~ "SSP2",
      scenario ==  "BaseGDPExo_msx_SSP3_5lab_labshock" ~ "SSP3")
  )

pop  %>%
  ggplot(aes(x = year, y = pop, group = scenario, color = scenario)) +
  geom_line() +
  labs(title = "Population")


# GDP ------------------------------------------------------------------------------------
gdp <- magnetr::magnet_indicator("gdp", scenario, period, base_year, magnet_path) %>%
  filter(region == iso3c_magnet) %>%
  rename(gdp = value) %>%
  mutate(
    scenario = case_when(
      scenario ==  "BaseGDPExo_msx_SSP1_5lab_labshock" ~ "SSP1",
      scenario ==  "BaseGDPExo_msx_SSP2_5lab_labshock" ~ "SSP2",
      scenario ==  "BaseGDPExo_msx_SSP3_5lab_labshock" ~ "SSP3")
  )

gdp_percap <- left_join(pop, gdp) %>%
  mutate(gdp_percap = gdp/pop)

gdp_percap  %>%
  ggplot(aes(x = year, y = gdp_percap, group = scenario, color = scenario)) +
  geom_line() +
  labs(title = "GDP per capita")


# EXOGENEOUS OCCUPATION PROJECTIONS --------------------------------------------------------
# Exogenous labour force projection (% in comparison to base year) per occupation
labf <- magnetr::magnet_scenario("LABF", shocks, period, file_path_shocks, "", "HAR") %>%
  dplyr::group_by(region, commodity, scenario) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(percent_cumulative = cumprod(1 + (value/100))) %>%
  filter(region == iso3c_magnet)

labf %>%
  ggplot(aes(x = year, y = percent_cumulative, group = scenario, color = scenario)) + facet_wrap(region ~ commodity, scales = "free") +
  geom_line() +
  labs(x = "Year", y = "LABF", title = "Labour") +
  scale_fill_discrete(name = "Scenario")


# WORKERS PER SECTOR AND OCCUPATION -------------------------------------------------------
qlab_occ_type <- magnetr::magnet_indicator("qlab", scenario, period, base_year, magnet_path) %>%
  dplyr::group_by(region, variable1, scenario, year) %>%
  summarise(qlab_per_occ = sum(value)) %>%
  mutate(
    scenario = case_when(
      scenario ==  "BaseGDPExo_msx_SSP1_5lab_labshock" ~ "SSP1",
      scenario ==  "BaseGDPExo_msx_SSP2_5lab_labshock" ~ "SSP2",
      scenario ==  "BaseGDPExo_msx_SSP3_5lab_labshock" ~ "SSP3")
  ) %>%
  filter(region == iso3c_magnet)

qlab_occ_type %>%
  ggplot(aes(x = year, y = qlab_per_occ, group = scenario, color = scenario)) +
  facet_wrap(region~ variable1, scales = "free") +
  geom_line() +
  labs(x = "Year", y = "Labour quantity", title = "Labour quantity") +
  scale_fill_discrete(name = "Scenario")


# ========================================================================================
# NET WAGE -------------------------------------------------------------------------------
# ========================================================================================

net_wage <- magnetr::magnet_indicator("net_wage", scenario, period, base_year, magnet_path) %>%
  rename(variable = variable1) %>%
  filter(region == iso3c_magnet)

# Evos: total labour cost
# CHECK relative wages clerks vs Techasp, seems incorrect.
# Patterns are weird!
net_wage_occ_type <- net_wage %>%
  group_by(variable, region, scenario, year) %>%
  summarise(wage = weighted.mean(value, qlab),
            .groups = "drop") %>%
  mutate(
    scenario = case_when(
      scenario ==  "BaseGDPExo_msx_SSP1_5lab_labshock" ~ "ssp1",
      scenario ==  "BaseGDPExo_msx_SSP2_5lab_labshock" ~ "ssp2",
      scenario ==  "BaseGDPExo_msx_SSP3_5lab_labshock" ~ "ssp3"),
  ) %>%
  group_by(variable, scenario, region) %>%
  mutate(index = wage/wage[year == 2014]) %>%
  ungroup()

net_wage_occ_type %>%
  ggplot(aes(x = year, y = index, group = scenario, color = scenario)) +
  facet_wrap(~ variable, scales = "free") +
  geom_line() +
  labs(x = "Year", y = "Net wage", title = "Net wage") +
  scale_fill_discrete(name = "Scenario")

net_wage_sector <- net_wage %>%
  group_by(commodity, region, scenario, year) %>%
  summarise(net_income = weighted.mean(value, qlab)) %>%
  filter(region == iso3c_magnet)

net_wage_sector %>%
  ggplot(aes(x = year, y = net_income, group = scenario, color = scenario)) + facet_wrap(~ commodity, scales = "free") +
  geom_line() +
  xlab("Year") + ylab("Net wage") +
  scale_fill_discrete(name = "Scenario")

# PREPARE MODEL INPUT --------------------------------------------------------------------

wage_proj <- net_wage_occ_type %>%
  dplyr::select(-wage) %>%
  rename(occupation = variable, value = index) %>%
  mutate(year = as.integer(year),
         occupation = case_when(
           occupation == "AgOthLs" ~ "ag_othlowsk",
           occupation == "Clerks" ~ "clerks",
           occupation == "ServShop" ~ "service_shop",
           occupation == "TechAsp" ~ "tech_aspros",
           occupation == "OffMgrPr" ~ "off_mgr_pros"),
         region = toupper(region)) %>%
  rename(adm0_code = region)


# Add missing classes. We assume that income change for not_in_lf and p65 is equal to ag_othlowsk
# and for m15 equal to 1.
wage_proj <- bind_rows(
  wage_proj,
  wage_proj %>%
    filter(occupation == "ag_othlowsk") %>%
    mutate(occupation = "not_in_lf"),
  wage_proj %>%
    filter(occupation == "ag_othlowsk") %>%
    mutate(occupation = "p65"),
  wage_proj %>%
    filter(occupation == "ag_othlowsk") %>%
    mutate(occupation = "m15",
           value = 1))

# Interpolate and create income proj index with base_year = 1
wage_proj <- wage_proj %>%
  complete(occupation, scenario, adm0_code, year = c(2014:2050)) %>%
  mutate(value = na_interpolation(value)) %>%
  group_by(occupation, scenario) %>%
  mutate(occ_wage_proj = value/value[year == param$base_year]) %>%
  dplyr::select(-value) %>%
  ungroup()


# PRICES ---------------------------------------------------------------------------------
price_cons_good_market_price <- magnetr::magnet_indicator("price_cons_good_market_price", scenario, period, base_year, magnet_path) %>%
  filter(region == iso3c_magnet)  %>%
  mutate(
    scenario = case_when(
      scenario ==  "BaseGDPExo_msx_SSP1_5lab_labshock" ~ "ssp1",
      scenario ==  "BaseGDPExo_msx_SSP2_5lab_labshock" ~ "ssp2",
      scenario ==  "BaseGDPExo_msx_SSP3_5lab_labshock" ~ "ssp3")
  ) %>%
  filter(region == iso3c_magnet)


price_cons_good_market_price %>%
  ggplot(aes(x = year, y = price_cons_good_market_price, group = scenario, color = scenario)) +
  facet_wrap(~ commodity, scales = "free") +
  geom_line() +
  labs(x = "Year", y = "Market price", title = "Market price") +
  scale_fill_discrete(name = "Scenario")

price_index <- price_cons_good_market_price %>%
  group_by(region, scenario, year) %>%
  summarize(price_index = weighted.mean(price_cons_good_market_price, vpb_volume),
            .groups = "drop")

price_index %>%
  ggplot(aes(x = year, y = price_index, group = scenario, color = scenario)) +
  geom_line() +
  labs(x = "Year", y = "Market price", title = "Market price") +
  scale_fill_discrete(name = "Scenario")


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(wage_proj, file.path(param$model_path, glue("simulation/wage_proj_{param$iso3c}.rds")))
saveRDS(price_cons_good_market_price, file.path(param$model_path, glue("simulation/price_proj_{param$iso3c}.rds")))
