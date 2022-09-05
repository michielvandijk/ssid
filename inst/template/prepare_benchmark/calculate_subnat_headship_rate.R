# ========================================================================================
# Project:  simFNS
# Subject:  process number of households data
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
p_load(ipumsr, sf, janitor)

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

# ipums
ddi <- read_ipums_ddi(file.path(raw_path, glue("ipums/{iso3c_sel}/2007/ipumsi_00002.xml")))
ipums_raw <- read_ipums_micro(ddi, verbose = FALSE)

# adm1 map consistent with microsim
# NOTE THAT WE USE ADM1 HERE BUT THIS IS NOT ALWAYS THE CASE! ALSO CHECH BELOW WHEN NAMES ARE MATCHED.
adm <- readRDS(file.path(proc_path, glue("adm/adm1_{iso3c_sel}.rds")))


# ========================================================================================
# HARMONIZE ADM --------------------------------------------------------------------------
# ========================================================================================

# Process IPUMS
ipums <- ipums_raw %>%
  mutate(
    adm1_name = as.character(as_factor(GEO1_ET2007)),
    adm2_name = as.character(as_factor(GEO2_ET2007))) %>%
  dplyr::select(-WERDET, -GEO1_ET2007, -GEO2_ET2007) %>%
  clean_names() %>%
  droplevels

# Convert all labels to factors
ipums <- ipums %>%
  mutate(across(where(is.labelled), as_factor))

# check match in adm1 names
adm_match <- adm %>%
  st_drop_geometry() %>%
  transmute(adm1_name, adm1_code, source1 = "adm") %>%
  full_join(ipums %>%
              dplyr::select(adm1_name) %>%
              unique() %>%
              mutate(source2 = "ipums")
  )

# Special region is not listed as one of the regions in the lsms-isa and most
# likely part of the Afar region (see adm construction). The census only includes
# a very small number of people. We merge the target regions and allocate them to
# Affar
ipums <- ipums %>%
  mutate(
    adm1_name =  case_when(
      adm1_name == "Southern Nations, Nationalities, and People (SNNP)" ~ "SNNP",
      adm1_name == "Special region" ~ "Affar",
      adm1_name == "Gambella" ~ "Gambela",
      adm1_name == "Oromiya" ~ "Oromia",
      TRUE ~ adm1_name))  %>%
  left_join(adm %>%
              st_drop_geometry()) 
  

# ========================================================================================
# CALCULATE HEADSHIP RATE ----------------------------------------------------------------
# ========================================================================================

# The sample fraction can be found on the IPUMS website:
# https://international.ipums.org/international-action/samples
sample_fraction <- 1/0.1 

# We use 10 year age groups to keep it simple
# We calculate the headship rate at adm1 as number of observations at adm2 is limited in some cases
# Note that for m15 the headship rate is Inf as there are no heads in this age group.
headship_rate <- ipums %>%
  mutate(
    age = fct_relabel(age2, ~ gsub(" to ", "_", .x)),
    age = fct_collapse(age,
                       "m15" = c("0_4", "5_9", "10_14"),
                       "15_24" = c("15_19", "20_24"),
                       "25_34" = c("25_29", "30_34"),
                       "35_44" = c("35_39", "40_44"),
                       "45_54" = c("45_49", "50_54"),
                       "55_64" = c("55_59", "60_64"),
                       "p65" = c("65_69", "70_74", "75_79", "80+", "85+")
    ),
    sex = fct_recode(sex, m = "Male", f = "Female"),
    head = ifelse(relate == "Head", "Y", "N")
  ) %>%
  droplevels() %>%
  group_by(age, sex, adm1_name, adm1_code) %>%
  summarize(
    n_head = sum(head == "Y"),
    n = n(),
    h = n/n_head,
    .groups = "drop"
  ) %>%
  mutate(h = ifelse(is.infinite(h), 0, h))

sum(headship_rate$n_head)
hist(headship_rate$h, breaks = 50)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "benchmark"), recursive = TRUE, showWarnings = FALSE)
saveRDS(headship_rate, file.path(proc_path,
                               glue("benchmark/headship_rate_{iso3c_sel}.rds")))

