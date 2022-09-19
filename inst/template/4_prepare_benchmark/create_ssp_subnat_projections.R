# ========================================================================================
# Project:  ssid
# Subject:  prepare subnational age_sex projections
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

# WDI Population
wdi <- readRDS(file.path(param$db_path, glue("wdi/wdi.rds"))) %>%
  filter(adm0_code == param$iso3c)

# UN Population by age and sex
un_male <- read_excel(file.path(
  param$db_path,
  "population_projections/un_population_prospects/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx"
  ),
  sheet = "ESTIMATES", skip = 16
  )

un_female <- read_excel(file.path(
  param$db_path,
  "population_projections/un_population_prospects/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx"
  ),
  sheet = "ESTIMATES", skip = 16
  )

# UN urbanization
un_urban <- read_excel(file.path(
  param$db_path,
  "population_projections/un_urbanization_prospects/WUP2018-F03-Urban_Population.xls"
  ),
  sheet = "Data", skip = 16
  )

un_rural <- read_excel(file.path(
  param$db_path,
  "population_projections/un_urbanization_prospects/WUP2018-F04-Rural_Population.xls"
  ),
  sheet = "Data", skip = 16
  )

# Subnational ssp population projections
subnat_urban_rural_proj_raw <- readRDS(file.path(
  param$model_path,
  glue("benchmark/subnat_urban_rural_proj_raw_{param$iso3c}.rds")
))

# Subnational age-sex base year
subnat_age_sex_by_raw <- readRDS(file.path(
  param$model_path,
  glue("benchmark/subnat_age_sex_by_raw_{param$iso3c}.rds")
))

# ssp database
ssp_raw <- read_csv(file.path(param$db_path, "ssp/ssp_v2/SspDb_country_data_2013-06-12.csv")) %>%
  clean_names() %>%
  remove_empty()

# Subnational occupation data
subnat_occ_by_raw <- readRDS(file.path(param$model_path,
                              glue("benchmark/subnat_emp_by_raw_{param$iso3c}.rds")))

# Macro employment projections
nat_occ_raw <- readRDS(file.path(param$db_path, glue("occupation_projections/occupation_projections.rds"))) %>%
  filter(iso3c  == param$iso3c) %>%
  rename(variable = occ)

# Headship rate
headship_rate <- readRDS(file.path(param$model_path,
                                 glue("benchmark/headship_rate_{param$iso3c}.rds")))


# ========================================================================================
# PREPARE NATIONAL AGE-SEX PROJECTIONS ---------------------------------------------------
# ========================================================================================

# PROCESS UN DATA ------------------------------------------------------------------------
nat_age_sex <- bind_rows(
  un_male %>%
    mutate(sex = "m"),
  un_female %>%
    mutate(sex = "f")
) %>%
  filter(`Country code` == countrycode(param$iso3c, "iso3c", "un")) %>%
  dplyr::select(-Index, -Variant, -`Region, subregion, country or area *`, -`Country code`,
    -`Parent code`, -Type,
    year = `Reference date (as of 1 July)`, -Notes
  ) %>%
  pivot_longer(-c(year, sex), names_to = "age", values_to = "value") %>%
  mutate(
    age = case_when(
      age %in% c("0-4", "5-9", "10-14") ~ "m15",
      age %in% c("15-19", "20-24") ~ "15_24",
      age %in% c("25-29", "30-34") ~ "25_34",
      age %in% c("35-39", "40-44") ~ "35_44",
      age %in% c("45-49", "50-54") ~ "45_54",
      age %in% c("55-59", "60-64") ~ "55_64",
      age %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") ~ "p65"
    ),
    value = as.numeric(value) * 1000,
    adm0_code = param$iso3c
  ) %>%
  group_by(adm0_code, year, sex, age) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

# Interpolate
nat_age_sex <- nat_age_sex %>%
  complete(adm0_code, sex, age, year = c(max(year):min(year))) %>%
  mutate(value = na_interpolation(value))


# PREPARE SSP POPULATION PROJECTIONS -------------------------------------------------
# Set variables to select
var_sel <- c(
  paste0("Population|Male|Aged", seq(0, 95, 5), "-", seq(4, 99, 5)),
  "Population|Male|Aged100+",
  paste0("Population|Female|Aged", seq(0, 95, 5), "-", seq(4, 99, 5)),
  "Population|Female|Aged100+"
)

nat_age_sex_proj <- ssp_raw %>%
  filter(variable %in% var_sel, region %in% param$iso3c) %>%
  mutate(scenario = substr(scenario, 1, 4)) %>%
  separate(variable, into = c("variable", "sex", "age"), sep = "\\|", remove = T) %>%
  dplyr::select(-model, -variable, -region) %>%
  pivot_longer(!c(scenario, sex, age, unit), names_to = "year", values_to = "value") %>%
  mutate(
    year = as.integer(gsub("x", "", year)),
    sex = if_else(sex == "Male", "m", "f"),
    age = gsub("Aged", "", age),
    age = gsub("-", "_", age),
    age = case_when(
      age %in% c("0_4", "5_9", "10_14") ~ "m15",
      age %in% c("15_19", "20_24") ~ "15_24",
      age %in% c("25_29", "30_34") ~ "25_34",
      age %in% c("35_39", "40_44") ~ "35_44",
      age %in% c("45_49", "50_54") ~ "45_54",
      age %in% c("55_59", "60_64") ~ "55_64",
      age %in% c("65_69", "70_74", "75_79", "80_84", "85_89", "90_94", "95_99", "100+") ~ "p65"
    ),
    value = value * 1000000,
    scenario = tolower(scenario)
  ) %>%
  group_by(scenario, sex, age, year) %>%
  summarize(value = sum(value, na.rm = T), .groups = "drop") %>%
  filter(scenario %in% c("ssp1", "ssp2", "ssp3"), year >= 2010)

# We linearly interpolate all values between 2010 and param$end_year for the SSP scenarios
# Interpolate
nat_age_sex_proj <- nat_age_sex_proj %>%
  complete(scenario, sex, age, year = c(2010:param$end_year)) %>%
  mutate(value = na_interpolation(value))


# COMPARE NATIONAL POPULATION DATA ----------------------------------------------------

# Compare WDI, UN and SSP national population projections
# UN and WDI populations are identical for years that both are presented.
# Very small differences occur for the years that are interpolated in the UN dataset.
# We keep the UN series as we require consistency with the age-sex projections.
nat_pop_comp <- bind_rows(
  nat_age_sex_proj %>%
    group_by(year, scenario) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "SSP"),
  wdi %>%
    dplyr::select(year, value = SP.POP.TOTL) %>%
    mutate(
      source = "WDI",
      scenario = "historical"
    ),
  nat_age_sex %>%
    group_by(adm0_code, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      source = "UN",
      scenario = "historical"
    ),
) %>%
  mutate(value = value / 1000000) %>%
  filter(year <= param$end_year)

ggplot(data = nat_pop_comp) +
  geom_line(aes(x = year, y = value, color = scenario, linetype = source))


# SCALE SSP PROJECTIONS TO UN POPULATION DATA FOR START YEAR FOR EACH AGE-SEX CLASS

# Calculate index
nat_age_sex_proj <- nat_age_sex_proj %>%
  group_by(scenario, sex, age) %>%
  mutate(index = value / value[year == param$base_year]) %>%
  ungroup()

# Add base year values from UN population data and recalculate projections
nat_age_sex_proj <- nat_age_sex_proj %>%
  left_join(
    nat_age_sex %>%
      filter(year == param$base_year) %>%
      dplyr::select(-year) %>%
      rename(by_value = value)) %>%
  mutate(value = by_value * index) %>%
  dplyr::select(-index, -by_value) %>%
  filter(year >= param$base_year,  year <= param$end_year)

# Compare again
nat_pop_comp2 <- bind_rows(
  nat_age_sex_proj %>%
    group_by(year, scenario) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "SSP"),
  wdi %>%
    dplyr::select(year, value = SP.POP.TOTL) %>%
    mutate(
      source = "WDI",
      scenario = "historical"
    ),
  nat_age_sex %>%
    group_by(adm0_code, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      source = "UN",
      scenario = "historical"
    ),
) %>%
  mutate(value = value / 1000000)

ggplot(data = nat_pop_comp2) +
  geom_line(aes(x = year, y = value, color = scenario, linetype = source))

# Create national aggregate values
nat_pop_proj <- nat_age_sex_proj %>%
  group_by(year, scenario) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

# Only select SSP1-3
nat_pop_proj <- nat_pop_proj %>%
  filter(scenario %in% c("ssp1", "ssp2", "ssp3"))


# ========================================================================================
# PREPARE BASE YEAR SUBNAT AGE-SEX DATA --------------------------------------------------
# ========================================================================================

# We use the worldpop subnat age-sex distribution in the base year and apply this to the
# UN by population total.

# Comparison between UN population and worldpop in base year
nat_age_sex %>%
  filter(year == param$base_year) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  pull()
sum(subnat_age_sex_by_raw$value)


# SCALE WORLDPOP TO NATIONAL AGE-SEX DISTRIBUTION ----------------------------------------

# Process worldpop
# Note that: 5 for age group 5 to 9 years, 00 = age group 0 to 12months,
# 01 = age group 1 to 4 years, 80 = age 80 years and over.

subnat_age_sex_by <- subnat_age_sex_by_raw %>%
  mutate(
      age = case_when(
      age %in% c("0", "1", "5", "10") ~ "m15",
      age %in% c("15", "20") ~ "15_24",
      age %in% c("25", "30") ~ "25_34",
      age %in% c("35", "40") ~ "35_44",
      age %in% c("45", "50") ~ "45_54",
      age %in% c("55", "60") ~ "55_64",
      age %in% c("65", "70", "75", "80") ~ "p65"
    )) %>%
  group_by(year, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, age, sex) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop")

# scale
subnat_age_sex_by <- subnat_age_sex_by %>%
  group_by(age, sex) %>%
  mutate(share = value/sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(
    nat_age_sex_proj %>%
      filter(scenario == "ssp2", year == param$base_year) %>%
      rename(value_by = value)
  ) %>%
  mutate(value = share * value_by) %>%
  dplyr::select(-value_by, -share)


# ========================================================================================
# PREPARE SUBNAT URBAN-RURAL PROJECTIONS -------------------------------------------------
# ========================================================================================

# PREPARE UN URBANIZATION DATA -----------------------------------------------------------
un_urban_rural <- bind_rows(
  un_urban %>%
    mutate(urban_rural = "urban"),
  un_rural %>%
    mutate(urban_rural = "rural")
) %>%
  filter(`Country\ncode` == countrycode(param$iso3c, "iso3c", "un")) %>%
  dplyr::select(
    -Index, -`Region, subregion, country or area`, -`Country\ncode`,
    -Note
  ) %>%
  pivot_longer(-c(urban_rural), names_to = "year", values_to = "value") %>%
  mutate(
    year = as.integer(year),
    value = value * 1000
  )

# Interpolate un urban-rural
un_urban_rural <- un_urban_rural %>%
  complete(urban_rural, year = c(min(year):max(year))) %>%
  mutate(value = na_interpolation(value))


# SCALE UN URBANIZATION DATA TO NATIONAL POP PROJECTIONS -----------------------------------
# UN urbanization data is consistent with UN population data but might slightly differ
# from the population projections as we might use interpolated values for the base year

# Calculate scaling factor
un_urban_sf <- bind_rows(
  nat_pop_proj %>%
    group_by(year, scenario) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "nat_pop_proj") %>%
    filter(scenario == "ssp2"),
  un_urban_rural %>%
    group_by(year) %>%
    summarize(
      value = sum(value, na.rm = TRUE),
      source = "un_urbanization"
    )
) %>%
  filter(year == param$base_year) %>%
  dplyr::select(-scenario) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  mutate(un_urban_sf = nat_pop_proj / un_urbanization)

# scale un urbanization prospects for base year
un_urban_rural <- un_urban_rural %>%
  filter(year == param$base_year) %>%
  mutate(value = value * un_urban_sf$un_urban_sf)


# SCALE BASE YEAR SUBNAT URBANIZATION VALUES TO NAT URBANIZATION AND SUBNAT AGE-SEX ---------

# Interpolate subnat urban-rural
subnat_urban_rural_proj <- subnat_urban_rural_proj_raw %>%
  filter(year %in% c(2010:param$end_year)) %>%
  complete(nesting(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, urban_rural),
           year = c(2010:param$end_year)
  ) %>%
  mutate(value = na_interpolation(value))

# Use RAS to scale base year urbanization values.
# We use SSP2 projections for the base year as starting point and scale them to
# the urbanization prospects and the subnational age-sex base year values.

# Note that ipu and ipu_matrix cannot handle numeric adm codes
# Hence we temporarily add a character

m <- subnat_urban_rural_proj %>%
    mutate(adm2_code = paste0("x", adm2_code)) %>%
    filter(year == param$base_year, scenario == "ssp2") %>%
    dplyr::select(adm2_code, urban_rural, value) %>%
    arrange(adm2_code, urban_rural) %>%
    pivot_wider(names_from = urban_rural, values_from = value, values_fill = 0) %>%
    column_to_rownames("adm2_code")
m <- as.matrix(m)

row_t <- subnat_age_sex_by %>%
    mutate(adm2_code = paste0("x", adm2_code)) %>%
    group_by(year, scenario, adm1_name, adm2_name, adm2_code) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(adm2_code) %>%
    pull(value)

col_t <- un_urban_rural %>%
    arrange(urban_rural) %>%
    pull(value)

all.equal(sum(col_t), sum(row_t))

subnat_urban_rural_by <- ipfr::ipu_matrix(m, row_t, col_t, verbose = TRUE)
subnat_urban_rural_by <- as.data.frame(subnat_urban_rural_by) %>%
  rownames_to_column("adm2_code") %>%
  pivot_longer(-adm2_code, names_to = "urban_rural", values_to = "value_by") %>%
  mutate(adm2_code = gsub("x", "", adm2_code))
rm(m, row_t, col_t)


# LINK URBAN-RURAL PROJECTIONS TO BASE YEAR ----------------------------------------------

# Calculate index
# For a very small number of adms the ssp base year values are 0 (e.g. no urban population)
# while future values are non-zero (e.g urbanization starting in 2030).
# This creates problems as the index number cannot be calculated.
# We solve this by now by setting the projections to 1 for all years in the adms where this happens.
# This ensures that the index is 1 throughout, base year values of the projections will be the same as the base year value
# and series are consistent.

# Check where things go wrong
index_nan_id <- subnat_urban_rural_proj %>%
  filter(year == param$base_year, value == 0) %>%
  mutate(id = paste(adm2_code, scenario, urban_rural, sep = "_")) %>%
  dplyr::select(id) %>%
  unique() %>%
  pull

index_nan <- subnat_urban_rural_proj %>%
  filter(paste(adm2_code, scenario, urban_rural, sep = "_") %in% index_nan_id)

# Replace projections, update using index and select param$base_year and param$end_year range
subnat_urban_rural_proj <- bind_rows(
  subnat_urban_rural_proj %>%
    filter(paste(adm2_code, scenario, urban_rural, sep = "_") %in% index_nan_id) %>%
    mutate(value = 1),
  subnat_urban_rural_proj %>%
    filter(!paste(adm2_code, scenario, urban_rural, sep = "_") %in% index_nan_id)
  ) %>%
  group_by(scenario, adm1_name, adm2_name, adm2_code, urban_rural) %>%
  mutate(index = value/value[year == param$base_year]) %>%
  ungroup() %>%
  left_join(subnat_urban_rural_by) %>%
  mutate(value = index*value_by) %>%
  filter(year >= param$base_year, year <= param$end_year) %>%
  dplyr::select(-value_by, -index)


# ========================================================================================
# SUBNATIONAL AGE-SEX PROJECTIONS --------------------------------------------------------
# ========================================================================================

# Calculate nat projections index
nat_age_sex_proj_index <- nat_age_sex_proj %>%
  group_by(scenario, sex, age) %>%
  mutate(index = value/value[year == param$base_year]) %>%
  ungroup() %>%
  dplyr::select(-value)

# Apply growth rates to subnat age-sex base year
subnat_age_sex_proj <- subnat_age_sex_by %>%
  dplyr::select(-year, -scenario) %>%
  left_join(nat_age_sex_proj_index) %>%
  mutate(value = index * value) %>%
  dplyr::select(-index)

# Visualize for random (first) adm to check
subnat_age_sex_proj %>%
  dplyr::filter(adm2_name == unique(subnat_age_sex_proj$adm2_name)[1]) %>%
  ggplot(aes(x = year, y = value)) +
  geom_area(aes(fill = age)) +
  facet_grid(scenario ~ sex)

# Compare totals with urban_rural total
comp_subnat_pop_proj <- bind_rows(
  subnat_age_sex_proj %>%
    group_by(year, scenario, adm0_code, adm1_name, adm2_name) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "subnat"),
  subnat_urban_rural_proj %>%
    filter(urban_rural %in% c("rural", "urban")) %>%
    group_by(year, scenario, adm0_code, adm1_name, adm2_name) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "urban_rural")
)

comp_subnat_pop_proj %>%
  dplyr::filter(adm2_name == unique(subnat_age_sex_proj$adm2_name)[20]) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(color = source)) +
  facet_grid(adm2_name ~ scenario, scales = "free")


# SELECT RELEVANT YEARS ------------------------------------------------------------------

# Select relevant ssps and years, include base year and base year of sub-national occupation data!
subnat_urban_rural_proj <- subnat_urban_rural_proj %>%
  filter(
    year %in% c(param$base_year, 2020, 2030, 2040, 2050))

# Combinations of year and scenarios for which matrix will be raked
ssp_year_all <- expand_grid(
  scenario = unique(subnat_urban_rural_proj$scenario),
  year = unique(subnat_urban_rural_proj$year)
)

# Use iterative proportional fitting to ensure totals add up to urban_rural total population projections
# Run parallel
availableCores()
plan(multisession, workers = 8)

with_progress({
  p <- progressor(steps = nrow(ssp_year_all))
  subnat_age_sex_proj <- future_map2_dfr(ssp_year_all$year, ssp_year_all$scenario, ipu_subnat_proj)
})

# Stop parallel
plan(sequential)

# Check if totals add up to subnational urban-rural totals
check_total_ur <- full_join(
  subnat_age_sex_proj %>%
    group_by(adm2_code, scenario, year) %>%
    summarize(raked = sum(value, na.rm = T), .groups = "drop"),
  subnat_urban_rural_proj %>%
    group_by(adm2_code, scenario, year) %>%
    summarize(ur = sum(value, na.rm = T), .groups = "drop") %>%
    filter(year %in% ssp_year_all$year, scenario %in% ssp_year_all$scenario)
)
all.equal(check_total_ur$raked, check_total_ur$ur)

# Check if totals add up to national age and sex totals
check_total_nat <- full_join(
  subnat_age_sex_proj %>%
    group_by(age, sex, scenario, year) %>%
    summarize(raked = sum(value, na.rm = T), .groups = "drop"),
  nat_age_sex_proj %>%
    group_by(age, sex, scenario, year) %>%
    summarize(nat = sum(value, na.rm = T), .groups = "drop") %>%
    filter(year %in% ssp_year_all$year, scenario %in% ssp_year_all$scenario)
)
all.equal(check_total_nat$raked, check_total_nat$nat)

# Add geo information
subnat_age_sex_proj <- subnat_age_sex_proj %>%
  full_join(subnat_urban_rural_proj %>%
    dplyr::select(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code) %>%
    distinct()) %>%
  dplyr::select(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, age, sex, year, value)


# ========================================================================================
# PREPARE NATIONAL OCCUPATION PROJECTIONS ------------------------------------------------
# ========================================================================================

# Calculate shares and growth index rate for shares
nat_occ <- nat_occ_raw %>%
  mutate(variable = ifelse(variable == "agri_lowsk", "ag_othlowsk", variable)) %>%
  filter(year >= param$base_year) %>%
  group_by(year, scenario) %>%
  mutate(share = value / sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(scenario, variable) %>%
  mutate(index = share / share[year == param$base_year]) %>%
  dplyr::select(-share, -value) %>%
  ungroup()

ggplot(data = nat_occ, aes(x = year, y = index)) +
  geom_line(aes(color = variable, linetype = scenario)) +
  facet_wrap(~variable, scales = "free")


# ========================================================================================
# SUBNATIONAL OCCUPATION PROJECTIONS -----------------------------------------------------
# ========================================================================================

# SUBNATIONAL WORKING AGE PROJECTIONS ----------------------------------------------------
# We group adm2 population into the following mutually exclusive categories:
# children (m15), elderly (p65) and working population (15-64)
subnat_wa_proj <- subnat_age_sex_proj %>%
  mutate(age = case_when(
    age %in% c(
      "15_24", "25_34", "35_44", "45_54", "55_64"
    ) ~ "working_age",
    TRUE ~ age
  )) %>%
  group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year, age) %>%
  summarize(population = sum(value, na.rm = TRUE), .groups = "drop")

# SUBNATIONAL OCCUPATION PROJECTIONS -----------------------------------------------------

# We use the national projections to project the subnational adm1 base year values
# We assume base year subnational values hold for simulation year
# We rescale projections so total sums to 1

subnat_occ_sh_proj <- subnat_occ_by_raw %>%
  filter(indicator == "occupation") %>%
  dplyr::select(variable, share = value, adm0_code, adm1_name) %>%
  left_join(nat_occ) %>%
  mutate(
    occ_proj = share * index,
    labor_force = "lf"
  ) %>%
  group_by(scenario, adm0_code, adm1_name, year) %>%
  mutate(occ_share = occ_proj / sum(occ_proj, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(scenario, year, adm0_code, adm1_name, occ = variable, labor_force, occ_share)

# We use the national projections to project the subnational adm1 base year values
# We assume base year subnational values hold for simulation year
# We rescale projections so total sums to 1
subnat_occ_proj <- subnat_wa_proj %>%
  left_join(subnat_occ_by_raw  %>%
              rename(labor_force = variable, lf_share = value) %>%
              mutate(age = "working_age") %>%
              filter(indicator == "employment") %>%
              dplyr::select(-year, -indicator)) %>%
  left_join(subnat_occ_sh_proj) %>%
  mutate(
    occ = case_when(
      age == "m15" ~ "m15",
      age == "p65" ~ "p65",
      age == "working_age" & is.na(occ_share) ~ "not_in_lf",
      age == "working_age" & !is.na(occ_share) ~ occ
    ),
    value = case_when(
      age == "m15" ~ population,
      age == "p65" ~ population,
      age == "working_age" & is.na(occ_share) ~ population * lf_share,
      age == "working_age" & !is.na(occ_share) ~ population * lf_share * occ_share
    )
  ) %>%
  dplyr::select(-labor_force, -lf_share, -occ_share, -age, -population)


# ========================================================================================
# CHECK FOR EQUALITY OF SUBNATIONAL PROJECTIONS ------------------------------------------
# ========================================================================================

check <- bind_rows(
  subnat_urban_rural_proj %>%
    group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "urban_rural"),
  subnat_age_sex_proj %>%
    group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "age_sex"),
  subnat_occ_proj %>%
    group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "wa")
) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(year != 2010, scenario == "ssp2") %>%
  mutate(check = urban_rural / wa)

all.equal(check$urban_rural, check$age_sex)
all.equal(check$urban_rural, check$wa)
all.equal(check$wa, check$age_sex)


# ========================================================================================
# NUMBER OF HOUSEHOLD PROJECTIONS --------------------------------------------------------
# ========================================================================================

# Calculate number of households
subnat_hh_proj <- subnat_age_sex_proj %>%
  group_by(scenario, year, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, sex, age) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(headship_rate)

# The sample fraction can be found on the IPUMS website:
# https://international.ipums.org/international-action/samples
sample_fraction <- 1/0.05

# Compare census and estimated hh_n
# Note that we compare census year with base year, which can be >10 years apart
n_hh_check <- subnat_hh_proj %>%
  mutate(n_hh = value * h, na.rm = T) %>%
  group_by(scenario, year, adm1_name, age, sex) %>%
  summarize(
    n_head = sample_fraction * unique(n_head),
    n_hh = sum(value * 1/h, na.rm = T),
    .groups = "drop"
  ) %>%
  filter(year == param$base_year, scenario == "ssp2") %>%
  mutate(check = n_hh / n_head)

# Calculate n_hh
# We filter out agr groups with 0 (<15) headship rate
subnat_hh_proj <- subnat_hh_proj %>%
  filter(h != 0) %>%
  mutate(n_hh = value * 1/h) %>%
  group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, year, scenario) %>%
  summarize(value = sum(n_hh, na.rm = T), .groups = "drop")


# ========================================================================================
# SUBTRACT M15 CATEGORY ------------------------------------------------------------------
# ========================================================================================

# Subnat household projections
# There are no headship rates for m15 so no need to recalculate number of households
headship_rate %>%
  filter(age == "m15", h != 0)
subnat_hh_proj_m15 <- subnat_hh_proj

# Subnat sex age projections
subnat_age_sex_proj_m15 <- subnat_age_sex_proj %>%
  filter(age != "m15")

# Subnat occupation projections
subnat_occ_proj_m15 <- subnat_occ_proj %>%
  filter(occ != "m15")

# Subnat urban-rural projections
# We subtract the m15 category. We allocate m15 to urban/rural using share of urban/rural in total
m15 <- subnat_age_sex_proj %>%
  filter(age == "m15") %>%
  group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
  summarize(m15 = sum(value, na.rm = T), .groups = "drop")

subnat_urban_rural_proj_m15 <- subnat_urban_rural_proj %>%
  group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
  mutate(u_r_share = value / sum(value, na.rm = T)) %>%
  ungroup() %>%
  left_join(m15) %>%
  mutate(value = value - (u_r_share * m15)) %>%
  dplyr::select(-m15, -u_r_share)


# CHECK FOR EQUALITY ---------------------------------------------------------------------

check_m15 <- bind_rows(
  subnat_urban_rural_proj_m15 %>%
    group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "urban_rural"),
  subnat_age_sex_proj_m15 %>%
    group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "age_sex"),
  subnat_occ_proj_m15 %>%
    group_by(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code, scenario, year) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "wa")
) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(year != 2010, scenario == "ssp2") %>%
  mutate(check = urban_rural / wa)

all.equal(check_m15$urban_rural, check_m15$age_sex)
all.equal(check_m15$urban_rural, check_m15$wa)
all.equal(check_m15$wa, check_m15$age_sex)



# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

# Subnat urban-rural projections
saveRDS(subnat_urban_rural_proj, file.path(
  param$model_path,
  glue("benchmark/subnat_urban_rural_proj_{param$iso3c}.rds")
))
saveRDS(subnat_urban_rural_proj_m15, file.path(
  param$model_path,
  glue("benchmark/subnat_urban_rural_proj_m15_{param$iso3c}.rds")
))

# Subnat sex age projections
saveRDS(subnat_age_sex_proj, file.path(
  param$model_path,
  glue("benchmark/subnat_age_sex_proj_{param$iso3c}.rds")
))
saveRDS(subnat_age_sex_proj_m15, file.path(
  param$model_path,
  glue("benchmark/subnat_age_sex_proj_m15_{param$iso3c}.rds")
))

# Subnat occupation projections
saveRDS(subnat_occ_proj, file.path(
  param$model_path,
  glue("benchmark/subnat_occupation_proj_{param$iso3c}.rds")
))
saveRDS(subnat_occ_proj_m15, file.path(
  param$model_path,
  glue("benchmark/subnat_occupation_proj_m15_{param$iso3c}.rds")
))

# Subnat household projections
saveRDS(subnat_hh_proj, file.path(
  param$model_path,
  glue("benchmark/subnat_hh_proj_{param$iso3c}.rds")
))
saveRDS(subnat_hh_proj_m15, file.path(
  param$model_path,
  glue("benchmark/subnat_hh_proj_m15_{param$iso3c}.rds")
))

