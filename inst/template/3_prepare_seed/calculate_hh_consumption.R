# ========================================================================================
# Project:  simFNS
# Subject:  Calculate hh consumption
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


# NOTES ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# Daily consumption
con1_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/HH_SEC_9A2.sav")))

# Weekly consumption
con2_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/HH_SEC_9B2.sav")))


# Monthly non-food consumption
con3_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/HH_SEC_9C.sav")))

# Annual non-food consumption and durables
con4_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/HH_SEC_9D1.sav")))

con5_raw <- read_sav(file.path(param$db_path,
                               glue("household_surveys/{param$continent}/{param$iso3c}/hies_2016/data/HH_SEC_9D2.sav")))

# povcalnet data
povcal_raw <- readRDS(file.path(param$db_path, glue("povcalnet/povcal.rds")))

# WDI data
wdi_raw <- readRDS(file.path(param$db_path, glue("wdi/wdi.rds")))


# ========================================================================================
# NOTES ----------------------------------------------------------------------------------
# ========================================================================================

# NB according to the manual, one hh 545041 is missing from the consumption data.
# We need to check for each consumption data component of it is available for a hh and
# remove hh from the seed for which data is missing (or impute)


# TOUPDATE: check if values are NA or 0 while q >0 if so impute using median prices corrected for outliers.
# HIES 2016 standardizes unit values and removes them if they are 2.5 below/above the mean
# Is this the same as removing the 2.5 quantile? CHECK In the end replaced by median.


# ========================================================================================
# FOOD CONSUMPTION -----------------------------------------------------------------------
# ========================================================================================

# DAILY FOOD CONSUMPTION FOR 14 DAY PERIOD -----------------------------------------------

con1 <- con1_raw %>%
  transmute(hh_id = hhold, day = day, item_code = as.numeric(item), item = as_factor(item), quantity = s9a2q02,
            unit = as_factor(s9a2q03), value = s9a2q04, other = s9a2_os, type = "daily food")

# Remove negative values and NAs
con1 <- con1 %>%
  filter(value >= 0) %>%
  filter(!is.na(item))

# List of items
item_list <- con1 %>%
  dplyr::select(item) %>%
  distinct()

# Remove aggregates to aggregate bottom up
con1 <- con1 %>%
  filter(!item_code %in% c(10, 30, 40, 60, 70, 80, 100, 110, 120, 130,
                      150, 160, 170, 180, 200))

# Identify combinations where quantity is 0 and value > 0 or reverse
check_q <- con1 %>%
  filter((quantity > 0 & (value == 0 | is.na(value)) | ((quantity == 0 | is.na(quantity)) & value > 0)))
rm(check_q)
# Calculate median unit values

# Multiply by 365/14 to obtain annual values
con1 <- con1 %>%
  mutate(value = value * (365/14))
summary(con1)
unique(sort(con1$item_code))


# WEEKLY FOOD CONSUMPTION FOR 14 DAY PERIOD ----------------------------------------------
con2 <- con2_raw %>%
  transmute(hh_id = hhold, week = week, item_code = as.numeric(s9bq01), item = as_factor(s9bq01), quantity = s9bq02,
          unit = as_factor(s9b2q03), value = s9bq04, type = "weekly food")

# Remove negative values and NAs
con2 <- con2 %>%
  filter(value >= 0) %>%
  filter(!is.na(item))

# Remove aggregates to aggregate bottom up
con2 <- con2 %>%
  filter(!item_code %in% c(210, 230))

# Multiply by 365/14 to obtain annual values
con2 <- con2 %>%
  mutate(value = value * (365/14))
summary(con2)
unique(sort(con2$item_code))


# ========================================================================================
# NON-FOOD CONSUMPTION -------------------------------------------------------------------
# ========================================================================================

# NOTES ----------------------------------------------------------------------------------
# Includes education, housing, daily use items, clothing, transport
# If possible uses an annual rental equivalent for durable goods and housing
# Excludes health expenses, gifts and remittances, lumpy items (e.g. birth, wedding
# and funeral related) and income and commodity taxes.

# See Deaton and Zaidi (2002), Guidelines for Constructing Consumption Aggregates
# for Welfare Analysis, LSM135, World Bank and and Lanjouw (2009), Constructing a
# Consumption Aggregate for the Purpose of Welfare Analysis: Principles, Issues
# and Recommendations Arising from the Case of Brazil, note for guidelines.
# ----------------------------------------------------------------------------------------

# TOUPDATE: impute rental rate for durables!! See page 34 Deaton and Zaidi
# Analyze which items are lumpy!
# Compare health expenditures from Section 3 and education expenditures from Section 2.
# In 2016 HIES, these are used when no costs where reported in the consumption category.
# AT THE MOMENT WE INCLUDE ALL ANNUAL ITEMS, NEED TO CLEAN!

# MONTHLY NON-FOOD CONSUMPTION -----------------------------------------------------------

# TOPUDATE Remove 3 digit codes to facilitate selection of codes and aggregation in consumption components.
# Might want to use change in prices from MAGNET to recalculate expenditure on education and health.

# Ensure that codes are three digit
con3 <- con3_raw %>%
  transmute(hh_id = hhold, item_code = as.numeric(s9cq00), item = as_factor(s9cq00), value = s9cq03,
            type = "monthly food")

# Remove negative values and NAs
con3 <- con3 %>%
  filter(value >= 0) %>%
  filter(!is.na(item))

# Remove aggregates to aggregate bottom up
con3 <- con3 %>%
  filter(!item_code %in% c(240, 250, 260, 270, 280))

# Multiply by 12 to obtain annual values
con3 <- con3 %>%
  mutate(value = value * 12)
summary(con3)
unique(sort(con3$item_code))


# ANNUAL NON-FOOD CONSUMPTION A ------------------------------------------------------------
con4 <- con4_raw %>%
  transmute(hh_id = hhold, item_code = as.numeric(s9d1q00), item = as_factor(s9d1q00),
            value = s9d1q02, type = "non-food")

# Remove aggregates to aggregate bottom up
con4 <- con4 %>%
  filter(!item_code %in% c(300, 330, 340))

# Remove negative values and NAs
con4 <- con4 %>%
  filter(value >= 0) %>%
  filter(!is.na(item))
summary(con4)
unique(sort(con4$item_code))


# ANNUAL NON-FOOD CONSUMPTION B ------------------------------------------------------------
con5 <- con5_raw %>%
  transmute(hh_id = hhold, item_code = as.numeric(s9d2q00), item = as_factor(s9d2q00),
            value = s9d2q01, type = "non-food")

# Remove aggregates to aggregate bottom up
con5 <- con5 %>%
  filter(!item_code %in% c(360, 380, 390, 410, 430, 440, 450, 470, 490, 500, 510, 520, 530, 550))

# Remove negative values and NAs
con5 <- con5 %>%
  filter(value >= 0) %>%
  filter(!is.na(item))
summary(con5)
unique(sort(con5$item_code))

# Remove health expenditures
con5 <- con5 %>%
  filter(!item_code %in% c(400:413, 420:434))

# Remove housing expenditures => TAX => CHECK
con5 <- con5 %>%
  filter(!item_code %in% c(398))

# Remove remittances, gifts and ceremonies
con5 <- con5 %>%
  filter(!item_code %in% c(451, 452, 453, 454, 455, 456, 457, 458, 459, 461, 462, 463))

# Remove remittances and non-regular expenditures (e.g. marriage, etc.)
con5 <- con5 %>%
  filter(!item_code %in% c(460:473))

# Remove taxes
con5 <- con5 %>%
  filter(!item_code %in% c(500:508))

# Remove durables
con5 <- con5 %>%
  filter(!item_code %in% c(510:556))

# Remove insurance => CHECK


# food_expenditure
# - use farm gate prices for produced on the farm consumption

# Food aggregate
# Can perhaps be used for a diversity score
# Also has information on food consumed away from home, which might be used as correction/addition!


# ========================================================================================
# DURABLES -------------------------------------------------------------------------------
# ========================================================================================

# PART OF CON5, NEED TO ADD


# ========================================================================================
# TOTAL CONSUMPTION ----------------------------------------------------------------------
# ========================================================================================

# AGGREGATE CONSUMPTION COMPONENTS ----------------------------------------------------
hh_con <- bind_rows(
  con1, con2, con3, con4, con5) %>%
  group_by(hh_id) %>%
  summarize(hh_income = sum(value, na.rm = TRUE),
            .groups = "drop")
summary(hh_con)
summary(con5)


# ========================================================================================
# MOVE SURVEY TO COMMON BASE YEAR --------------------------------------------------------
# ========================================================================================

#' In order to make household consumption comparable in time and space we:
#'
#' 1. Use the WDI CPI to convert consumption to the PPP base year used by the World Bank in
#' povcalnet.
#'
#' 2. Convert income to PPP USD using the World Bank PPP used in povcalnet.
#'
#' 3. Project the PPP USD consumption data to by_sel by using the growth rate in
#' mean per capita income from the survey_year ot by_sel from povcalnet.
#'
#' The result is consumption expressed in PPP USD for our base year (by_sel).
#'
#' We also use the growth in mean per capita income (in PPP USD but this is the same as in LC as
# both are in real terms) to project household income in lc to the base year. This estimates
# can be compared with national poverty line if it is also expressed in survey year real terms


# PREPARE WDI ----------------------------------------------------------------------------
ppp_year <- 2011
wdi <- wdi_raw %>%
  mutate(across(everything(), zap_label)) %>%
  filter(adm0_code == param$iso3c) %>%
  dplyr::select(year, FP.CPI.TOTL, PA.NUS.PRVT.PP) %>%
  mutate(
    cpi = FP.CPI.TOTL / FP.CPI.TOTL[year == param$base_year],
    ppp = PA.NUS.PRVT.PP[year == ppp_year]
  )

# PREPARE POVCAL -------------------------------------------------------------------------

povcal <- povcal_raw %>%
  filter(adm0_code == param$iso3c) %>%
  mutate(income_pc_growth = mean/mean[year == param$base_year])


# CONVERT CONSUMPTION --------------------------------------------------------------------

hh_con <- hh_con %>%
  mutate(
    hh_income_pppy_lc = hh_income/wdi$cpi[wdi$year == ppp_year],
    hh_income_pppy_usd = hh_income_pppy_lc/wdi$ppp[wdi$year == ppp_year],
    hh_income_by_usd = hh_income_pppy_usd * povcal$income_pc_growth[povcal$year == param$base_year],
    hh_income_by_lc = hh_income * povcal$income_pc_growth[povcal$year == param$base_year]
  )
summary(hh_con)

# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(con1_raw, con1, con2_raw, con2, con3_raw, con3, con4_raw, con4, con5_raw, con5,
   ppp_year, povcal, povcal_raw, wdi, wdi_raw)
