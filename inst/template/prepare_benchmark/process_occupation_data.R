# ========================================================================================
# Project:  sidd
# Subject:  process occupation data
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here("working_paper/scripts/model_setup/set_model_parameters.r"))



# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "BGD"

# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
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

