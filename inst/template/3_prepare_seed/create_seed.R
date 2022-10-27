# ========================================================================================
# Project:  ssid
# Subject:  Script to prepare seed
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("working_paper/scripts/1_model_setup/set_model_parameters.r"))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# NOTES ----------------------------------------------------------------------------------
#' In case of .dta files use code below to create factors
#' %>%
#'   mutate(across(where(is.labelled), as_factor))
#'
# ----------------------------------------------------------------------------------------

# Household consumption
source(here("Working_paper/scripts/3_prepare_seed/calculate_hh_consumption.r"))

# adm data
adm <- read_sav(file.path(param$db_path,
                          glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/hh_sec_a.sav")))
adm_list_map <- readRDS(file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))


# Household and person data
hh1 <- read_sav(file.path(param$db_path,
                          glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/hh_sec_a.sav")))
hh2 <- read_sav(file.path(param$db_path,
                          glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/hh_sec_1a.sav")))

# labour data
lab1 <- read_sav(file.path(param$db_path,
                           glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/hh_sec_1b.sav")))
lab2 <- read_sav(file.path(param$db_path,
                           glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/hh_sec_4a.sav")))
hies2isco <- read_excel(file.path(param$db_path,
                                  glue("conversion_tables/{param$iso3c}/hies2isco_2016_bgd.xlsx")))


# ========================================================================================
# ADM LIST -------------------------------------------------------------------------------
# ========================================================================================

# Create adm list
# We fix a few adm2 names to match with the BGD polygon file
# PSA (municipalities) are coded as urban: https://en.wikipedia.org/wiki/List_of_Municipal_Corporations_in_Bangladesh
adm_list <- adm %>%
  select(
    hh_id = hhold,
    adm1_name = id_01_name,
    adm2_name = id_02_name,
    urban_rural = ruc) %>%
  mutate(adm2_name = case_when(
    adm2_name == "BRAHMANBARIA" ~ "BRAHAMANBARIA",
    adm2_name == "KISHOREGONJ" ~ "KISHOREGANJ",
    adm2_name == "CHAPAI NABABGANJ" ~ "NAWABGANJ",
    TRUE ~ adm2_name),
    adm0_code = param$iso3c) %>%
  mutate(urban_rural = case_when(
    urban_rural %in% c("2", "3") ~ "urban",
    urban_rural %in% c("1") ~ "rural")
  ) %>%
  left_join(adm_list_map)

# Create unique hh_id and per_id if needed


# ========================================================================================
# PERSON LIST -----------------------------------------------------------------------------
# ========================================================================================

# NOTES ----------------------------------------------------------------------------------
# Check whether there is a question of the household member was abroad, travelling for most
# of the year, was a temporary visitor, etc. These members should be removed.
#
# In this case there is a question on being more than six months abroad
# ----------------------------------------------------------------------------------------


# Main characteristics
per_list <- hh2 %>%
  dplyr::select(hh_id = hhold, per_id = s1aq00,
                sex = s1aq01, age_raw = s1aq03,
                exclude = s1aq08) %>%
  filter(exclude != 1) %>%
  mutate(
    sex = factor(if_else(sex == 1, "m", "f")),
    age = cut(age_raw,
              include.lowest = TRUE,
              right = FALSE,
              breaks = c(0, seq(15, 65, 10), Inf),
              labels = c("m15", paste(seq(15, 60, 10),"_", seq(24, 64, 10), sep = ""), "p65")
    )
  ) %>%
  distinct

# Create unique hh_id and per_id if needed
per_list <- per_list %>%
  mutate(per_id = stringr::str_pad(per_id, 2, side = "left", pad = 0), # ensure two digits throughout
         per_id = glue("{hh_id}{per_id}"))


# Identify hhid for which person info is missing
hh_miss <- per_list %>%
  filter_all(any_vars(is.na(.)))

# Remove hh_id with missing information
per_list <- per_list %>%
  filter(!hh_id %in% hh_miss$hh_id)


# ========================================================================================
# HOUSEHOLD LIST -------------------------------------------------------------------------
# ========================================================================================

# Main characteristics
hh_list <- hh1 %>%
  dplyr::select(
    hh_id = hhold,
    ea_id = psu,
    survey_hh_weight = hhwgt) %>%
  distinct()

# Create unique hh_id and per_id if needed

# Add adm info and weight
hh_list <- hh_list %>%
  left_join(adm_list)

# Remove hh_id with missing information
hh_list <- hh_list %>%
  filter(!hh_id %in% hh_miss$hh_id)


# ========================================================================================
# HOUSEHOLD COMPOSITION ------------------------------------------------------------------
# ========================================================================================

# Calculate hh size and number of children (<15)
# Household composition
hh_comp <- per_list %>%
  group_by(hh_id) %>%
  summarize(hh_size = n(),
            hh_n_m15 = sum(age == "m15"), .groups = "drop")


# ========================================================================================
# EMPLOYMENT AND OCCUPATION --------------------------------------------------------------
# ========================================================================================

# EMPLOYMENT -----------------------------------------------------------------------------

# Determine whether is a person is employed, unemployed or not available for work.
# Definition of unemployment: people who are jobless, actively seeking work, and available to take a job.
# We assume that missing information on whether people worked means they are not in the labour force as
# in practice these are mostly children
per_emp <- lab1 %>%
  dplyr::select(hh_id = hhold, per_id = s1bq00,
                work = s1bq01, work_avail = s1bq02, work_look = s1bq03, work_avail_why = s1bq04) %>%
  mutate(employment = case_when(
    work %in% 1 ~ "employed",
    work %in% 2 & work_avail %in% 1 & work_look %in% 1 ~ "unemployed",
    TRUE ~ "not_in_lf"
  )) %>%
  dplyr::select(per_id, hh_id, employment)

# Create unique hh_id and per_id if needed
per_emp <- per_emp %>%
  mutate(per_id = stringr::str_pad(per_id, 2, side = "left", pad = 0), # ensure two digits throughout
         per_id = glue("{hh_id}{per_id}"))


# OCCUPATION -----------------------------------------------------------------------------

# Several entries do not have a per_id and occupation data, we remove them.
# We also remove a few observations wit per_id of 0, which is not possible and occupation of 0 (housewives)
per_occ <- lab2 %>%
  dplyr::select(hh_id = hhold, per_id = s4aq00,
                occ_survey = s4aq01b) %>%
  filter(!is.na(per_id)) %>%
  filter(!per_id == 0) %>%
  filter(!occ_survey == 0) %>%
  distinct()

# Create unique hh_id and per_id if needed
per_occ <- per_occ %>%
  mutate(per_id = stringr::str_pad(per_id, 2, side = "left", pad = 0), # ensure two digits throughout
         per_id = glue("{hh_id}{per_id}"))

# Add ILO occupation and recode all unmatching (only 0 for housewives to N)
per_occ <- per_occ %>%
  left_join(hies2isco) %>%
  dplyr::select(-description, -note)

# Several per_id are listed with multiple occupations.
duplicates <- per_occ %>%
  count(hh_id, per_id) %>%
  left_join(.,per_occ) %>%
  arrange(desc(n)) %>%
  filter(n > 1)

# We recode them, in this order, ag_othlowsk if listed, service_shop, clerks, off_mgr_pros, tech_aspros,
# if one of these is listed, and remove duplicates.
duplicates_clean <- duplicates %>%
  dplyr::select(-occ_survey) %>%
  group_by(hh_id, per_id) %>%
  mutate(
    occisco = case_when(
      any(occisco %in% c(6:9)) ~ 6,
      any(occisco %in% c(4)) ~ 4,
      any(occisco %in% c(1,2)) ~ 1,
      any(occisco %in% c(3)) ~ 3,
      TRUE ~ occisco
    )
  ) %>%
  distinct

# Put duplicates_clean back in the sample
per_occ <- bind_rows(
  per_occ %>%
    count(hh_id, per_id) %>%
    left_join(.,per_occ) %>%
    arrange(desc(n)) %>%
    filter(n == 1) %>%
    dplyr::select(-n, -occ_survey),
  duplicates_clean %>%
    dplyr::select(-n)
)

# Link per_list and add gtap_occ labels
per_occ <- left_join(per_list, per_occ) %>%
  mutate(
    occ_gtap = case_when(
      occisco %in% c(1,2) ~ "off_mgr_pros",
      occisco %in% c(3) ~ "tech_aspros",
      occisco %in% c(4) ~ "clerks",
      occisco %in% c(5) ~ "service_shop",
      occisco %in% c(6, 7, 8, 9) ~ "ag_othlowsk")
  )

# Create occupation classification
per_occ <- per_occ %>%
  mutate(occupation = case_when(
    age == "m15" ~ "m15",
    age == "p65" ~ "p65",
    !is.na(occ_gtap) ~ occ_gtap,
    TRUE ~ "not_in_lf")
  ) %>%
  dplyr::select(hh_id, per_id, sex, age, occupation)


# COMBINE -------------------------------------------------------------------------------

# As the measurement of employment, unemployment and not available and we assume that m15 and
# p65 or not part of the labour force by definition the table on occupation is leading.

per_occ_emp <- left_join(per_occ, per_emp) %>%
  mutate(employment = case_when(
    occupation %in% c("off_mgr_pros", "tech_aspros", "clerks", "service_shop", "ag_othlowsk") &
      employment %in% "employed" ~ "employed",
    occupation %in% c("off_mgr_pros", "tech_aspros", "clerks", "service_shop", "ag_othlowsk") &
      employment %in% "unemployed" ~ "unemployed",
    TRUE ~ occupation)
  )


# ========================================================================================
# COMBINE DATA ---------------------------------------------------------------------------
# ========================================================================================

# HOUSEHOLD LEVEL ------------------------------------------------------------------------

# Combine household data and remove households with missing data that still might exist
# because of inconsistencies between hh_list and per_list, or because of missing income data.
# Check why certain households are deleted!
hh_db <- hh_list %>%
  left_join(hh_comp) %>%
  left_join(hh_con) %>%
  zap_label() %>%
  na.omit()


# PERSON LEVEL ---------------------------------------------------------------------------

# Combine
per_db <- hh_list %>%
  filter(hh_id %in% hh_db$hh_id) %>%
  left_join(per_occ_emp) %>%
  zap_label() %>%
  na.omit()


# ========================================================================================
# REMOVE M15 ONLY HOUSEHOLDS--------------------------------------------------------------
# ========================================================================================

# We assume that children (m15) do not contribute to household income in the simulation.
# We run into problems if there is a household that consists only of m15 household members
# as we cannot simulate income for this type of hh. We remove them from the seed.

# One hh in this case
hh_m15 <- hh_db %>%
  filter(hh_n_m15 == hh_size)

# Remove hh_m15
hh_db <- hh_db %>%
  filter(!hh_id %in% hh_m15$hh_id)

per_db <- per_db %>%
  filter(!hh_id %in% hh_m15$hh_id)


# ========================================================================================
# CHECK ---------------------------------------------------------------------------------_
# ========================================================================================

# Is hh_id the same in hh_db and per_db
all.equal(sort(unique(hh_db$hh_id)), sort(unique(per_db$hh_id)))

# No missing values
nrow(per_db %>%
       filter_all(any_vars(is.na(.)))) == 0
nrow(hh_db %>%
       filter_all(any_vars(is.na(.)))) == 0


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

temp_path <- file.path(param$model_path, "seed")
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
saveRDS(hh_db, file.path(temp_path, glue("hh_db_{param$iso3c}.rds")))
saveRDS(per_db, file.path(temp_path, glue("per_db_{param$iso3c}.rds")))


# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(
  adm, hh1, hh2, lab1, lab2, hies2isco,
  adm_list, adm_list_map, hh_list, hh_miss, hh_m15, hh_comp, hh_con,
  duplicates, duplicates_clean, item_list,
  per_list, per_emp, per_occ, per_occ_emp,
  temp_path
)

