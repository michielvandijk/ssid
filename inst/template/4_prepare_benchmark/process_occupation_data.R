# ========================================================================================
# Project:  ssid
# Subject:  process occupation data
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

# Employment data
subnat_db <- readRDS(file.path(param$db_path, glue("snl_db/subnat_dhs.rds"))) %>%
  filter(adm0_code == param$iso3c)
nat_emp_raw <- readRDS(file.path(param$db_path, glue("snl_db/nat_dhs.rds")))
ilo_raw <- readRDS(file.path(param$db_path, glue("snl_db/nat_ilo.rds"))) %>%
  filter(iso3c == param$iso3c)

# adm
adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))


# ========================================================================================
# HARMONIZE ADM NAMES --------------------------------------------------------------------
# ========================================================================================

# Select adm occupation data by selecting base year or last available year
if(param$base_year == max(subnat_db$year == param$base_year)) {
  subnat_emp <- subnat_db %>%
    filter(year == param$base_year)
} else {
  subnat_emp <- subnat_db %>%
    filter(year == max(year)) %>%
    mutate(year = param$base_year)
  max(subnat_db$year)
}

# adm microsim
unique(sort(adm$adm1_name))

# adm dhs
sort(unique(subnat_emp$region))

# Remap subnat_emp to names used in microsim
subnat_emp <- subnat_emp %>%
  filter(!region == "Chittagong/Sylhet") %>%
  mutate(adm1_name = case_when(
    region == "..Chittagong" ~ "CHITTAGONG",
    region == "..Sylhet" ~ "SYLHET",
    region == "Barisal" ~ "BARISAL",
    region == "Dhaka before 2015" ~ "DHAKA",
    region == "Khulna" ~ "KHULNA",
    region == "Rajshahi/Rangpur" ~ "RAJSHAHI",
    TRUE ~ region
  )) %>%
  dplyr::select(-region)

# Assume that missing adms, which are the result of spliting adms, have the same values.
subnat_emp <- bind_rows(
  subnat_emp %>%
    filter(adm1_name == "DHAKA") %>%
    mutate(adm1_name = "MYMENSINGH"),
  subnat_emp %>%
    filter(adm1_name == "RAJSHAHI") %>%
    mutate(adm1_name = "RANGPUR"),
  subnat_emp
)


# ========================================================================================
# SPLIT DATA -----------------------------------------------------------------------------
# ========================================================================================

# DHS combines off_mgr_pros and off_mgr_pros in one category
# We split them using national shares from the ILO
# We use the same year as for the DHS or first available (manually at the moment)

# select occupation data
subnat_occ <- subnat_emp %>%
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
    mutate(source = "DHS"))

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
)

# COMBINE --------------------------------------------------------------------------------
subnat_emp <- bind_rows(
  subnat_occ,
  subnat_emp %>%
    filter(!variable %in% c("ag_othlowsk", "clerks", "off_mgr_pros",
                            "off_mgr_pros & tech_aspros", "service_shop"))
)

# ADD ADM2 CODE AND NAME IF NECESSARY ----------------------------------------------------
# Even in base year occupation is only available at adm1 level, add adm2 name and code
# in order to link data in the next step when benchmark projections are calculated

# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(subnat_emp, file.path(param$model_path,
                              glue("benchmark/subnat_emp_by_raw_{param$iso3c}.rds")))


# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(adm, comp_ilo_dhs, ilo, ilo_raw, ilo_split, nat_emp_raw, subnat_db, subnat_emp, subnat_occ)
