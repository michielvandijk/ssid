# ========================================================================================
# Project:  simFNS
# Subject:  Script to prepare seed
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
p_load(haven, labelled, janitor, sf)

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

# We turn all labels in factors and get rid of the variable label using zap_label

# 1. Household roster
hh_1 <- read_dta(file.path(raw_path, glue("household_surveys/{iso3c_sel}/ETH_2018_ESS_v02_M_Stata/sect1_hh_w4.dta"))) %>%
  mutate(across(where(is.labelled), as_factor))

# 2. Education
# Perhaps useful to project forward and as covariate to predict income

# 4. Time use and labor
hh_4 <- read_dta(file.path(raw_path, glue("household_surveys/{iso3c_sel}/ETH_2018_ESS_v02_M_Stata/sect4_hh_w4.dta"))) %>%
  mutate(across(where(is.labelled), as_factor))


# File with aggregate consumption
cons_raw <- read_dta(file.path(raw_path, glue("household_surveys/{iso3c_sel}/ETH_2018_ESS_v02_M_Stata/cons_agg_w4.dta")))

# adm1 map consistent with microsim
# NOTE THAT WE USE ADM1 HERE BUT THIS IS NOT ALWAYS THE CASE! ALSO CHECH BELOW WHEN NAMES ARE MATCHED.
adm <- readRDS(file.path(proc_path, glue("adm/adm1_{iso3c_sel}.rds")))


# ========================================================================================
# HOUSEHOLD DATABASE ---------------------------------------------------------------------
# ========================================================================================

# HOUSEHOLD LIST -------------------------------------------------------------------------

# Main characteristics
hh_list <- hh_1 %>%
  dplyr::select(hh_id = household_id, ea_id, survey_hh_weight = pw_w4, urban_rural = saq14,
                adm1_name = saq01) %>%
  distinct() %>%
  mutate(urban_rural = fct_recode(urban_rural, rural = "1. RURAL", urban = "2. URBAN"))


# check match in adm names
adm_match <- adm %>%
  st_drop_geometry() %>%
  transmute(adm1_name, adm1_code, source1 = "adm") %>%
  full_join(hh_list %>%
              dplyr::select(adm1_name) %>%
              mutate(source2 = "survey") %>%
              unique() 
  )

# Add standard adm1_name
hh_list <- hh_list %>%
  mutate(
    adm1_name = case_when(
      adm1_name ==  "1. TIGRAY" ~ "Tigray",
      adm1_name ==  "2. AFAR" ~ "Affar",
      adm1_name ==  "3. AMHARA" ~ "Amhara",
      adm1_name ==  "4. OROMIA" ~ "Oromia",
      adm1_name ==  "5. SOMALI" ~ "Somali",
      adm1_name ==  "6. BENISHANGUL GUMUZ" ~ "Benishangul-Gumuz",
      adm1_name ==  "7. SNNP" ~ "SNNP",
      adm1_name ==  "12. GAMBELA" ~ "Gambela",
      adm1_name ==  "13. HARAR" ~ "Harari",
      adm1_name ==  "14. ADDIS ABABA" ~ "Addis Ababa",
      adm1_name ==  "15. DIRE DAWA" ~ "Dire Dawa")
  ) %>%
  left_join(adm %>%
              st_drop_geometry()) 


# # HOUSEHOLD HEAD CHARACTERISTICS ---------------------------------------------------------
#
# hh_head <- hh_1 %>%
#   dplyr::select(household_id, individual_id, ea_id, hh_member = s1q01, sex = s1q02,
#                 age = s1q03a) %>%
#   filter(hh_member == "1. Head") %>%
#   distinct()
#
# # Recode to common names and labels
# hh_head <- hh_head %>%
#   mutate(head_sex = fct_recode(sex, m = "1. Male", f = "2. Female"),
#          head_age = cut(age,
#                         include.lowest = TRUE,
#                         right = FALSE,
#                         breaks = c(0, seq(15, 65, 5), Inf),
#                         labels = c("<15", paste(seq(15, 60, 5),"_", seq(19, 64, 5), sep = ""), "65+")
#          )
#   )
# tabyl(hh_head$head_age)
#
# # Clean up
# hh_head <- hh_head %>%
#   dplyr::select(household_id, ea_id, head_sex, head_age)
#

# HOUSEHOLD CHARACTERISTICS ------------------------------------------------------------

# household number. we add a dummy variable ('n') as we are only interested in the total number of hh.
hh_number <- hh_1 %>%
  dplyr::select(hh_id = household_id) %>%
  distinct() %>%
  mutate(hh_number = "n")

# Household composition
hh_comp <- hh_1 %>%
  dplyr::select(hh_id = household_id, per_id = individual_id, ea_id,
                hh_member = s1q01, sex = s1q02,
                age_raw = s1q03a, new_member = s1q04, still_member = s1q05) %>%
  mutate(
    sex = fct_recode(sex, m = "1. Male", f = "2. Female"),
    age = cut(age_raw,
              include.lowest = TRUE,
              right = FALSE,
              breaks = c(0, seq(15, 65, 5), Inf),
              labels = c("m15", paste(seq(15, 60, 5),"_", seq(19, 64, 5), sep = ""), "p65")
    )
  )


# Adjust family size accounting for new members (= kids born) and members who left the family
# We assume that members with NA for still-member are still part of the family
# We filter out household members that left and, hence, no further information is provided
hh_comp <- hh_comp %>%
  mutate(
    still_member = as.character(still_member),
    still_member = factor(ifelse(is.na(still_member), "1. YES", still_member))) %>%
  filter(still_member == "1. YES")


# Calculate hh size and number of children (<15)
# Household composition
hh_comp <- hh_comp %>%
  group_by(hh_id) %>%
  summarize(hh_size = n(),
            hh_n_m15 = sum(age == "m15"))

# %>%
#
#   pivot_wider(names_from = age2, values_from = n, values_fill = 0)


# INCOME/EXPENDITURE ---------------------------------------------------------------------

# Consumption data provided by lsms-isa. It is not clear how some values are calculated but we use them
# for now. Our preferred variable is the spatially adjusted per aeq consumption.
# Eventually, we have to calculcate cons ourselves bottom up...
# A comparison shows that hh_size_cons and hh_size as calculated below are identical. Hence we can use
# hh_size as measure of household size.

hh_income <- cons_raw %>%
  mutate(total_cons_spat = adulteq*spat_totcons_aeq,
         total_cons_nom = adulteq*nom_totcons_aeq) %>%
  dplyr::select(hh_id = household_id, adulteq, hh_size_cons = hh_size, total_cons_ann, total_cons_spat, total_cons_nom) %>%
  dplyr::select(-total_cons_nom, -total_cons_ann, -adulteq, -hh_size_cons) %>%
  rename(hh_income = total_cons_spat)


# COMBINE --------------------------------------------------------------------------------

# Combine and strip variable labels
hh_db <- hh_list %>%
  left_join(hh_number) %>%
  left_join(hh_comp) %>%
  left_join(hh_income) %>%
  mutate(across(everything(), zap_label))


# ========================================================================================
# HH MEMBERS -----------------------------------------------------------------------------
# ========================================================================================

# HOUSEHOLD MEMBER LIST -------------------------------------------------------------------------

# Main characteristics
per_list <- hh_1 %>%
  dplyr::select(hh_id = household_id, per_id = individual_id, ea_id,
                hh_member = s1q01, sex = s1q02,
                age_raw = s1q03a, new_member = s1q04, still_member = s1q05) %>%
  mutate(
    sex = fct_recode(sex, m = "1. Male", f = "2. Female"),
    age = cut(age_raw,
              include.lowest = TRUE,
              right = FALSE,
              breaks = c(0, seq(15, 65, 10), Inf),
              labels = c("m15", paste(seq(15, 60, 10),"_", seq(24, 64, 10), sep = ""), "p65")
    )
  )


# Adjust family size accounting for new members (= kids born) and members who left the family
# We assume that members with NA for still-member are still part of the family
# We filter out household members that left and, hence, no further information is provided
per_list <- per_list %>%
  mutate(
    still_member = as.character(still_member),
    still_member = factor(ifelse(is.na(still_member), "1. YES", still_member))) %>%
  filter(still_member == "1. YES")


# OCCUPATION -----------------------------------------------------------------------------

per_occ <- hh_4 %>%
  dplyr::select(hh_id = household_id, per_id = individual_id, ea_id, ag_work = s4q05,
                ag_work_h = s4q06, return_to_ag_work = s4q21, ag4sale = s4q07, paid_work = s4q12,
                emp_status = s4q33, occ_survey = s4q34b, sector = s4q34d)

# Add ILO occupation
# A small number of persons has paid work and report the ILO classification
per_occ <- left_join(per_list, per_occ) %>%
  mutate(
    occisco = case_when(
      occ_survey == "1. LEGISLATORS, SENIOR GOVERNMENT OFFICIALS AND MANAGERS" ~ 1,
      occ_survey == "2. PROFESSIONALS/ PHYSICAL, MATHEMATICAL AND ENGINEERING SCIENCE PROFESSIONALS" ~ 2,
      occ_survey == "3. TECHNICIANS AND ASSOCIATE PROFESSIONALS/ PHYSICAL AND ENGINEERING SCIENCE ASSOCIATE PROFESSIONALS" ~ 3,
      occ_survey == "4. CLERKS, OFFICE CLERKS" ~ 4,
      occ_survey == "5. SERVICE WORKERS AND SHOP AND MARKET SALES WORKERS/ PERSONAL AND PROTECTIVE SERVICE WORKERS, TRAVEL ATTENDANTS AND RELATED WORKERS" ~ 5,
      occ_survey == "6. SKILLED AGRICULTURAL AND FISHERY WORKERS MARKET-ORIENTED SKILLED AGRICULTURAL AND FISHERY WORKERS" ~ 6,
      occ_survey == "7. CRAFT AND RELATED TRADES WORKERS, EXTRACTION AND BUILDING TRADES WORKERS" ~ 7,
      occ_survey == "8. PLANT AND MACHINE OPERATORS AND ASSEMBLERS, STATIONARY-PLANT AND RELATED OPERATORS" ~ 8,
      occ_survey == "9. ELEMENTARY OCCUPATIONS, SALES AND SERVICES ELEMENTARY OCCUPATIONS" ~ 9,
      occ_survey == "10. ARMY/ MEMBER OF THE ARMED FORCES" ~ 10,
      TRUE ~ NA_real_),
    occisco = factor(occisco, labels= c(
      "Managers", "Professionals", "Technicians and Associate  Professionals", "Clerical Support Workers",
      "Services and Sales Workers", "Skilled Agricultural, Forestry and Fishery Workers", "Craft and Related Trades Workers",
      "Plant and Machine Operators, and Assemblers", "Elementary Occupations", "Armed Forces Occupations")),
    occ_gtap = case_when(
      occisco %in% c("Managers", "Professionals") ~ "off_mgr_pros",
      occisco %in% c("Technicians and Associate  Professionals") ~ "tech_aspros",
      occisco %in% c("Clerical Support Workers") ~ "clerks",
      occisco %in% c("Services and Sales Workers") ~ "service_shop",
      occisco %in% c("Skilled Agricultural, Forestry and Fishery Workers", "Craft and Related Trades Workers",
                     "Plant and Machine Operators, and Assemblers", "Elementary Occupations", "Armed Forces Occupations") ~ "ag_othlowsk"))

# Create occupation classification
# We assume that (a) all persons that conducted ag_work in the last seven days without an occ code and
# (b) all persons that indicated they will return to ag_work can be regarded as family farm workers.
# We added them to ag_othlowsk, which includes this group.
per_occ <- per_occ %>%
  mutate(occupation = case_when(
    age == "m15" ~ "m15",
    age == "p65" ~ "p65",
    !is.na(occ_gtap) ~ occ_gtap,
    ag_work == "1. YES" | return_to_ag_work ==  "1. YES" ~ "ag_othlowsk",
    TRUE ~ "not_in_lf")
  )


# COMBINE --------------------------------------------------------------------------------

# Combine and strip variable labels
per_db <- hh_list %>%
  left_join(per_occ) %>%
  mutate(across(everything(), zap_label)) %>%
  dplyr::select(hh_id, per_id, survey_hh_weight, urban_rural, adm1_name, sex, age, occupation)


# ========================================================================================
# REMOVE M15 ONLY HOUSEHOLDS------------------------------------------------------------
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
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

dir.create(file.path(proc_path, "simulation"), showWarnings = FALSE, recursive = TRUE)
saveRDS(hh_db, file.path(proc_path, glue("simulation/hh_db_{iso3c_sel}.rds")))
saveRDS(per_db, file.path(proc_path, glue("simulation/per_db_{iso3c_sel}.rds")))



