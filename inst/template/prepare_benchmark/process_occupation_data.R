# ========================================================================================
# Project:  simFNS
# Subject:  process occupation data
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
p_load(sf)

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET ISO3c AND BASE YEAR ----------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "ETH"
by_sel <- 2018


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Subnational employment data
subnat_db_raw <- readRDS(file.path(raw_path, glue("snl_db/subnat_dhs.rds"))) %>%
  filter(adm0_code == iso3c_sel) %>%
  rename(adm1_name = region) 

# ilo data
ilo_raw <- readRDS(file.path(raw_path, glue("snl_db/nat_ilo.rds"))) %>%
  filter(iso3c == iso3c_sel)

# adm1 map consistent with microsim
# NOTE THAT WE USE ADM1 HERE BUT THIS IS NOT ALWAYS THE CASE! ALSO CHECH BELOW WHEN NAMES ARE MATCHED.
adm <- readRDS(file.path(proc_path, glue("adm/adm1_{iso3c_sel}.rds")))


# ========================================================================================
# HARMONIZE YEAR AND ADM -----------------------------------------------------------------
# ========================================================================================

# Select adm occupation data by selecting base year or most recent year close to the base year
if(by_sel == max(subnat_db_raw$year)) {
  subnat_db <- subnat_db_raw %>%
    filter(year == by_sel) 
} else {
  subnat_db <- subnat_db_raw %>%
    filter(year == year[which.min(abs(year - by_sel))])
  print(max(subnat_db$year))
  subnat_db <- subnat_db %>%
    mutate(year = by_sel)
}

# check match in adm names
adm_match <- adm %>%
  st_drop_geometry() %>%
  transmute(adm1_name, adm1_code, source1 = "adm") %>%
  full_join(subnat_db %>%
               dplyr::select(adm1_name) %>%
               unique() %>%
               mutate(source2 = "occ")
  )

# Remap by_subnat_shares to names used in microsim
subnat_db <- subnat_db %>%
  mutate(adm1_name = case_when(
    adm1_name == "SNNPR" ~ "SNNP",
    adm1_name == "Afar" ~ "Affar",
    TRUE ~ adm1_name
  )) %>%
  left_join(adm %>%
              st_drop_geometry()) 


# ========================================================================================
# SPLIT DATA -----------------------------------------------------------------------------
# ========================================================================================

# DHS combines off_mgr_pros and off_mgr_pros in one category
# We split them using national shares from the ILO
# We use the same year as for the DHS or first available (manually at the moment)

# select occupation data
subnat_occ <- subnat_db %>%
  filter(variable %in% c("ag_othlowsk", "clerks", "off_mgr_pros & tech_aspros", "service_shop"))

# Calculate ILO shares
ilo <- ilo_raw %>%
  filter(urban_rural == "total", year == 2010, occ_gtap != "total") %>%
  mutate(value = value/sum(value, na.rm = TRUE)) %>%
  rename(variable = occ_gtap)

# Compare dhs and ilo
# Subnational DHS values resemble ILO national values
comp_ilo_dhs <- bind_rows(
  ilo %>%
    mutate(source = "ILO"),
  subnat_occ %>%
    mutate(source = "Subnational"))

ggplot(data = comp_ilo_dhs) +
  geom_point(aes(x = variable, y = value, shape = source, color = source),
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
    dplyr::select(-split),
  subnat_occ %>%
    filter(!variable %in% c("off_mgr_pros & tech_aspros"))
) %>%
  group_by(year, adm0_code, adm1_name) %>%
  mutate(share = value/sum(value, na.rm = TRUE)) %>%
  ungroup

# Compare dhs and ilo again
# Subnational DHS values resemble ILO national values
comp_ilo_dhs <- bind_rows(
  ilo %>%
    mutate(source = "ILO"),
  subnat_occ %>%
    mutate(source = "Subnational"))

ggplot(data = comp_ilo_dhs) +
  geom_point(aes(x = variable, y = value, shape = source, color = source),
             size = 2, alpha = 0.5)


# COMBINE --------------------------------------------------------------------------------
subnat_db <- bind_rows(
  subnat_occ,
  subnat_db %>%
    filter(!variable %in% c("ag_othlowsk", "clerks", "off_mgr_pros",
                            "off_mgr_pros & tech_aspros", "service_shop"))
)

# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "benchmark"), recursive = TRUE, showWarnings = FALSE)
saveRDS(subnat_db, file.path(proc_path,
                               glue("benchmark/subnat_occ_by_raw_{iso3c_sel}.rds")))

