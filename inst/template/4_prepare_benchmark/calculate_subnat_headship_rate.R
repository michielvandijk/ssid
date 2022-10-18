# ========================================================================================
# Project:  ssid
# Subject:  process number of households data
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

# The sample fraction can be found on the IPUMS website:
# https://international.ipums.org/international-action/samples
sample_fraction <- 1/0.05
ddi <- read_ipums_ddi(file.path(param$db_path, glue("ipums/{param$iso3c}/2011/ipumsi_00005.xml")))
ipums_raw <- read_ipums_micro(ddi, verbose = FALSE)

# adm
adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))


# ========================================================================================
# HARMONIZE ADM --------------------------------------------------------------------------
# ========================================================================================

# Process IPUMS
ipums <- ipums_raw %>%
  mutate(
    adm1_name = toupper(as.character(as_factor(GEO1_BD2011))),
    adm2_name = toupper(as.character(as_factor(GEO2_BD2011)))
    ) %>%
  clean_names() %>%
  droplevels

# Convert all labels to factors
ipums <- ipums %>%
  mutate(across(where(is.labelled), as_factor))


# COMPARE ADM NAMES ----------------------------------------------------------------------

# MYMENSINGH is not a separate adm in ipums
# adm 1
check_adm1 <- full_join(
  adm %>%
    st_drop_geometry() %>%
    dplyr::select(adm1_name) %>%
    mutate(source1 = "map") %>%
    unique(),
  ipums %>%
    dplyr::select(adm1_name) %>%
    mutate(source2 = "ipums") %>%
    unique()
)

# adm 2
check_adm2 <- full_join(
  adm %>%
    st_drop_geometry() %>%
    dplyr::select(adm1_name, adm2_name) %>%
    mutate(adm2_name = case_when(
      adm2_name == "BRAHAMANBARIA" ~ "BRAHMANBARIA",
      adm2_name == "JHENAIDAH" ~ "JHENAIDAHA",
      adm2_name == "MAULVIBAZAR" ~ "MAULVI BAZAR",
      adm2_name == "NAWABGANJ" ~ "CHAPAI NABABGANJ",
      adm2_name == "NETRAKONA" ~ "NETROKONA",
      TRUE ~ adm2_name),
      source1 = "map") %>%
    unique(),
  ipums %>%
    dplyr::select(adm2_name) %>%
    mutate(source2 = "ipums") %>%
    unique()
)

# ALIGN ADM ------------------------------------------------------------------------------

# Perfect match!. Now we want to know which adm2 units are part of MYMENSINGH.
mymensingh <- check_adm2 %>%
  filter(adm1_name == "MYMENSINGH") %>%
  dplyr::select(adm2_name) %>%
  pull()

ipums <- ipums %>%
  mutate(
    adm1_name = case_when(
      adm2_name %in% mymensingh ~ "MYMENSINGH",
      TRUE ~ adm1_name
    )
  )


# ========================================================================================
# CALCULATE HEADSHIP RATE ----------------------------------------------------------------
# ========================================================================================

# We use 10 year age groups to keep it simple
# We calculate the headship rate at adm1 as number of observations at adm2 is limited in some cases
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
  group_by(age, sex, adm1_name) %>%
  summarize(
    n_head = sum(head == "Y"),
    n = n(),
    h = n_head / n,
    .groups = "drop"
  )
sum(headship_rate$n_head)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(headship_rate, file.path(param$model_path,
                               glue("benchmark/headship_rate_{param$iso3c}.rds")))

