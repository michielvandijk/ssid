# ========================================================================================
# Project:  simFNS
# Subject:  Script to create subnational occupation projections
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


# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET ISO3c AND BASE YEAR ----------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"
by <- 2018

# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Subnational employment data
subnat_db <- readRDS(file.path(raw_path, glue("snl_db/subnat_dhs.rds"))) %>%
  filter(adm0_code == "ETH")

# Macro employment projections
nat_occ_raw <- readRDS(file.path(raw_path, glue("occupation_projections/occupation_projections.rds"))) %>%
   filter(iso3c  == iso3c_sel)

# ADM1 map consistent with microsim
adm1 <- readRDS(file.path(proc_path, glue("adm/adm1_{iso3c_sel}.rds")))

# Subnational population
subnat_pop <- readRDS(file.path(proc_path, glue("benchmark/subnat_urban_rural_proj_raw_{iso3c_sel}.rds")))

# ========================================================================================
# MAP SUBNATIONAL EMPLOYMENT DATA ---------------------------------------------------------
# ========================================================================================

# Select adm occupation data by selecting base year or last available year
if(by == max(subnat_db$year == by)) {
  by_subnat_shares <- subnat_db %>%
    filter(year == by) 
} else {
  by_subnat_shares <- subnat_db %>%
    filter(year == max(year)) %>%
    mutate(year = by)
  max(subnat_db$year)
}

# adm1 microsim
sort(adm1$adm1_name)

# adm1 dhs
sort(unique(by_subnat_shares$region))

# Remap by_subnat_shares to names used in microsim
by_subnat_shares <- by_subnat_shares %>%
  mutate(adm1_name = case_when(
    region == "Benishangul-Gumuz" ~ "Benishangul-Gumz\r\n",
    TRUE ~ region
  )) %>%
  dplyr::select(-region)


# ========================================================================================
# PREPARE NATIONAL OCCUPATION PROJECTIONS ------------------------------------------------
# ========================================================================================

# Calculate shares and growth index rate for shares
nat_occ <- nat_occ_raw %>%
  group_by(occ) %>%
  mutate(index = value / value[year == by]) %>%
  filter(year >= by) %>%
  dplyr::select(-value) %>%
  ungroup()

ggplot(data = nat_occ, aes(x = year, y = index)) +
  geom_line(aes(color = occ, linetype = scenario)) +
  facet_wrap(~occ, scales = "free")


# ========================================================================================
# SUBNATIONAL OCCUPATION PROJECTIONS -----------------------------------------------------
# ========================================================================================

# SUBNATIONAL WORKING AGE PROJECTIONS ----------------------------------------------------
# Select working age population
subnat_wa_proj <- subnat_age_sex_proj %>%
  mutate(age = case_when(
    age %in% c(
      "15_24", "25_34", "35_44", "45_54",
      "55_64"
    ) ~ "working_age",
    TRUE ~ age
  )) %>%
  group_by(adm1_name, adm1_code, adm2_name, adm2_code, adm_level, scenario, year, age) %>%
  summarize(population = sum(value, na.rm = TRUE), .groups = "drop")

# SUBNATIONAL OCCUPATION PROJECTIONS -----------------------------------------------------
# We use the national projections to project the subnational adm1 base year values
# We assume 2013 values at subnational values hold for 2014
# We project total (urban + rural) only
# We rescale projections so total sums to 1
subnat_occ_sh_proj <- subnat_occ %>%
  left_join(nat_occ) %>%
  mutate(
    occ_proj = occ_share * index,
    labor_force = "lf"
  ) %>%
  group_by(adm1_name, year, gender, urban_rural, labor_force) %>%
  mutate(occ_share = occ_proj / sum(occ_proj, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(year, adm1_name, occ, labor_force, occ_share)

# We group adm2 population into the following mutually exclusive categories:
# children (m15), elderly (p65)
# working population (15-64), which is divided employed people (split into five occupation classes)
# and people not in the labor force (including unemployed, housewives, students, etc)

# NB: need to be clear how we allocate self-employed family farmers.
# I would argue they are included ag_othlowsk in this case as this category captures this
# But it is not clear how they are counted in other data pieces.

subnat_occ_proj <- subnat_wa_proj %>%
  left_join(subnat_emp) %>%
  left_join(subnat_occ_sh_proj) %>%
  filter(year != 2010) %>%
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
# H1 -------------------------------------------------------------------------------------
# ========================================================================================
# Employment data
subnat_emp_raw <- readRDS(file.path(raw_path, glue("snl_db/subnat_dhs.rds"))) %>%
  filter(adm0_code == iso3c_sel)
nat_emp_raw <- readRDS(file.path(raw_path, glue("snl_db/nat_dhs.rds")))
ilo_raw <- readRDS(file.path(raw_path, glue("snl_db/nat_ilo.rds"))) %>%
  filter(iso3c == iso3c_sel)

# adm1
adm1 <- readRDS(file.path(proc_path, glue("adm/adm1_{iso3c_sel}.rds")))


# ========================================================================================
# PREPARE SUBNATIONAL EMPLOYMENT DATA ----------------------------------------------------
# ========================================================================================

# We select a year that is closes to the base year (manually at the moment)
subnat_emp <- subnat_emp_raw %>%
  filter(year == 2004)


# HARMONIZE ADM NAMES --------------------------------------------------------------------
# Remap to names used in adm1, select relevant variables
unique(adm1$adm1_name)
unique(subnat_emp_raw$region)

# Note: we do not use Chittagong/Sylhet, which is a combination of two regions.
subnat_emp <- subnat_emp %>%
  mutate(adm1_name = case_when(
    region == "..Chittagong" ~ "Chittagong",
    region == "..Sylhet" ~ "Sylhet",
    region == "Barisal" ~ "Barisal",
    region == "Khulna" ~ "Khulna",
    region == "Dhaka before 2015" ~ "Dhaka",
    region == "Rajshahi/Rangpur" ~ "Rajshahi, Rangpur")) %>%
  filter(!region == "Chittagong/Sylhet") %>%
  dplyr::select(-region)


# SPLIT DATA -----------------------------------------------------------------------------
# DHS combines off_mgr_pros and off_mgr_pros in one category
# We split them using national shares from the ILO
# We use the same year as for the DHS or first available (manually at the moment)

# select occupation data
subnat_occ <- subnat_emp %>%
  filter(variable %in% c("ag_othlowsk", "clerks", "off_mgr_pros & tech_aspros", "service_shop"))

# Calculate ILO shares
ilo <- ilo_raw %>%
  filter(urban_rural == "total", year == 2010, occ_gtap != "total") %>%
  mutate(share = value/sum(value, na.rm = TRUE)) %>%
  rename(variable = occ_gtap)

# Compare dhs and ilo
# Subnational DHS values resemble ILO national values
comp_ilo_dhs <- bind_rows(
  ilo %>%
    mutate(source = "ILO"),
  subnat_occ %>%
    mutate(source = "DHS"))

ggplot(data = comp_ilo_dhs) +
  geom_point(aes(x = variable, y = share, shape = source, color = source),
             size = 2, alpha = 0.5)

# Split subnational off_mgr_pros & tech_aspros
ilo_split <- ilo %>%
  filter(variable %in% c("off_mgr_pros", "tech_aspros")) %>%
  mutate(split = value/sum(value, na.rm = TRUE)) %>%
  dplyr::select(variable, split)

subnat_occ <- bind_rows(
  bind_rows(
    subnat_occ %>%
      filter(variable %in% c("off_mgr_pros & tech_aspros")) %>%
      mutate(variable = "off_mgr_pros"),
    subnat_occ %>%
      filter(variable %in% c("off_mgr_pros & tech_aspros")) %>%
      mutate(variable = "tech_aspros")
  ) %>%
    left_join(ilo_split) %>%
    mutate(value = value * split) %>%
    dplyr::select(-split, -share),
  subnat_occ %>%
    filter(!variable %in% c("off_mgr_pros & tech_aspros")) %>%
    dplyr::select(-share)
) %>%
  group_by(year, adm0_code, adm1_name) %>%
  mutate(share = value/sum(value, na.rm = TRUE)) %>%
  ungroup


# COMBINE --------------------------------------------------------------------------------
subnat_emp <- bind_rows(
  subnat_occ,
  subnat_emp %>%
    filter(!variable %in% c("ag_othlowsk", "clerks", "off_mgr_pros",
                            "off_mgr_pros & tech_aspros", "service_shop"))
)

# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "benchmark"), recursive = TRUE, showWarnings = FALSE)
saveRDS(subnat_emp, file.path(proc_path,
                              glue("benchmark/subnat_emp_by_raw_{iso3c_sel}.rds")))



