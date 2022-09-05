# ========================================================================================
# Project:  simFNS
# Subject:  Exploratory data analysis
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
} else {
  library(pacman)
}

# Load key packages
p_load("here", "tidyverse", "readxl", "stringr", "scales", "glue", "haven", "janitor",
       "texreg", "dotwhisker", "broom")

# Set path
source(here("working_paper/scripts/support/set_path.r"))

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET ISO3c ------------------------------------------------------------------------------
# ========================================================================================

iso3c_sel <- "BGD"


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Seed
hh_db <- readRDS(file.path(proc_path, glue("simulation/hh_db_{iso3c_sel}.rds")))
per_db <- readRDS(file.path(proc_path, glue("simulation/per_db_{iso3c_sel}.rds")))

# Adm_list
adm_list <- readRDS(file.path(proc_path, glue("adm/adm_list_{iso3c_sel}.rds")))

# Subnat urban-rural projections
subnat_urban_rural_proj <- readRDS(file.path(proc_path,
                                             glue("benchmark/subnat_urban_rural_proj_{iso3c_sel}.rds")))
subnat_urban_rural_proj_m15 <- readRDS(file.path(proc_path,
                                                 glue("benchmark/subnat_urban_rural_proj_m15_{iso3c_sel}.rds")))

# Subnat population projections
subnat_age_sex_proj <- readRDS(file.path(proc_path,
                                         glue("benchmark/subnat_age_sex_proj_{iso3c_sel}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(proc_path,
                                             glue("benchmark/subnat_age_sex_proj_m15_{iso3c_sel}.rds")))

# Subnat occupation projections
subnat_occ_proj <- readRDS(file.path(proc_path,
                                     glue("benchmark/subnat_occupation_proj_{iso3c_sel}.rds")))
subnat_occ_proj_m15 <- readRDS(file.path(proc_path,
                                         glue("benchmark/subnat_occupation_proj_m15_{iso3c_sel}.rds")))

# Subnat household projections
subnat_hh_proj <- readRDS(file.path(proc_path,
                                    glue("benchmark/subnat_hh_proj_{iso3c_sel}.rds")))
subnat_hh_proj_m15 <- readRDS(file.path(proc_path,
                                        glue("benchmark/subnat_hh_proj_m15_{iso3c_sel}.rds")))


# ========================================================================================
# PREPARATION ----------------------------------------------------------------------------
# ========================================================================================

# Link hh income with occ
hh_occ <- per_db %>%
  dplyr::select(hh_id, urban_rural, occupation) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = occupation, values_from = value, values_fn = sum, values_fill = 0) %>%
  left_join(hh_db)


# ========================================================================================
# COMPARE SEED WITH BENCHMARK PROJECTIONS-------------------------------------------------
# ========================================================================================

# For the model to converge, it is important to check if the households in the seed have
# the characteristics that are required by the benchmark. For example if, say in a poor
# rural area of a country there is only one household/person in the engineering occupation
# class, it will be very hard to find weights that satisfy a strong increase in engineers.
# Simularly, if there are only few urban households, it might be difficult to simulate
# urbanization.

# We aim to use the most detailed adm-level at which the household survey as seed, i.e.
# in case of the Ethiopia LSMS-ISA adm1. So in that case, we check the seed for each adm1.

# OCCUPATION -----------------------------------------------------------------------------

# We only look at high-skill occupations as these will be problematic
subnat_occ_proj %>%
  filter(occupation %in% c("clerks", "off_mgr_pros", "service_shop", "tech_aspros")) %>%
  filter(year %in% c(2018, 2050)) %>%
  ggplot() +
    geom_col(aes(x = adm1_name, y = value, fill = occupation)) +
    facet_wrap(~factor(year)) +
    theme(axis.text.x = element_text(angle = 90))

per_db  %>%
  group_by(adm1_name, occupation) %>%
  summarize(value = n(),
            .groups = "drop") %>%
  filter(occupation %in% c("clerks", "off_mgr_pros", "service_shop", "tech_aspros")) %>%
  ggplot() +
    geom_col(aes(x = adm1_name, y = value, fill = occupation)) +
    theme(axis.text.x = element_text(angle = 90))

subnat_occ_proj %>%
  filter(occupation %in% c("clerks", "off_mgr_pros", "service_shop", "tech_aspros")) %>%
  filter(year %in% c(2018)) %>%
  group_by(adm1_name, occupation, year) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = occupation, values_from = value)

per_db  %>%
  group_by(adm1_name, occupation) %>%
  summarize(value = n()) %>%
  filter(occupation %in% c("clerks", "off_mgr_pros", "service_shop", "tech_aspros")) %>%
  pivot_wider(names_from = occupation, values_from = value)


# URBAN-RURAL -----------------------------------------------------------------------------

# We only look at high-skill occupations as these will be problematic
subnat_urban_rural_proj %>%
  filter(year %in% c(2018, 2050)) %>%
  ggplot() +
    geom_col(aes(x = adm1_name, y = value, fill = urban_rural)) +
    facet_wrap(~factor(year)) +
    theme(axis.text.x = element_text(angle = 90))

per_db  %>%
  group_by(adm1_name, urban_rural) %>%
  summarize(value = n(),
            .groups = "drop") %>%
  #filter(occupation %in% c("clerks", "off_mgr_pros", "service_shop", "tech_aspros")) %>%
  ggplot() +
    geom_col(aes(x = adm1_name, y = value, fill = urban_rural)) +
    theme(axis.text.x = element_text(angle = 90))

subnat_urban_rural_proj %>%
  filter(year %in% c(2018, 2050)) %>%
  group_by(adm1_name, urban_rural, year) %>%
  summarize(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = urban_rural, values_from = value)

per_db  %>%
  group_by(adm1_name, urban_rural) %>%
  summarize(value = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = urban_rural, values_from = value)


# ========================================================================================
# OCCUPATION AND INCOME ANALYSIS ---------------------------------------------------------
# ========================================================================================

# INDEPENDENT VARIABLES ------------------------------------------------------------------

# Distribution
per_db %>%
  count(occupation) %>%
  ggplot() +
  geom_bar(aes(x = reorder(occupation, -n), y = n, fill = occupation), stat = "identity")

# DEPENDENT VARIABLE ---------------------------------------------------------------------

# Distribution
summary(hh_occ$hh_income)
hh_occ %>%
  ggplot() +
  geom_histogram(aes(x = hh_income), binwidth = 10000)

hh_occ %>%
  ggplot() +
  geom_histogram(aes(x = log(hh_income)))

# Inspect outliers
hh_occ %>%
  ggplot() +
  geom_histogram(aes(x = hh_income), binwidth = 10000) +
  coord_cartesian(ylim = c(0, 50))

# Outliers seem data entry errors as occupation is not unusual.
# Need to inspect hh_income calculation to see what is going on
outliers <- hh_occ %>%
  filter(hh_income > 2000000)


# OCCUPATION ---------------------------------------------------------------------------
# Occ distribution by urban_rural
# Shows that high-earning jobs are located in urban areas
per_db %>%
  group_by(urban_rural, occupation) %>%
  count %>%
  ggplot() +
  geom_col(aes(x = occupation, y = n, fill = occupation)) +
  facet_wrap(~urban_rural) +
  coord_flip()

# Occ distribution by adm1_name
# Shows that for some adm1 several occupations hardly occur, meaning we cannot estimate
# income per adm1_name unless we combine adm1 regions
per_db %>%
  group_by(adm1_name, occupation) %>%
  count %>%
  ggplot() +
  geom_col(aes(x = occupation, y = n, fill = occupation)) +
  facet_wrap(~adm1_name) +
  coord_flip()

# CONS -----------------------------------------------------------------------------------
# Consumption by urban_rural
# Shows that highest-consumption/income is located in urban areas
hh_db %>%
  group_by(urban_rural) %>%
  summarize(value = mean(hh_income, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = urban_rural, y = value, fill = urban_rural))

hh_db %>%
  group_by(urban_rural, adm1_name) %>%
  summarize(value = mean(hh_income, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot() +
  geom_col(aes(x = urban_rural, y = value, fill = urban_rural)) +
  facet_wrap(~adm1_name)

# CORRELATION & RELATIONS ----------------------------------------------------------------
# https://paulvanderlaken.com/2018/09/10/simpler-correlation-analysis-in-r-using-tidyverse-principles/
# Correlation but probably not very useful in this case
hh_occ %>%
  dplyr::select(ag_othlowsk, clerks, m15, not_emp, off_mgr_pros, p65, service_shop, tech_aspros, hh_income) %>%
  corrr::correlate() %>%
  corrr::focus(hh_income) %>%
  ggplot() +
  geom_bar(aes(x = reorder(term, -hh_income), y = hh_income, fill = term), stat = "identity")


# ========================================================================================
# OCCUPATION REGRESSION ------------------------------------------------------------------
# ========================================================================================
# We follow Hallegate (2017) and we used weighted regression to relate family income
# (proxied by consumption) to occupation for each household member, excluding the intercept.

# NATIONAL -------------------------------------------------------------------------------
# baseline
b <- lm(hh_income ~ not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
          clerks + tech_aspros,
        data = hh_occ)

b_ni <- lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
             clerks + tech_aspros,
           data = hh_occ)

# weighted, no transformation
w <- lm(hh_income ~ not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
          clerks + tech_aspros,
        data = hh_occ,
        weights =  survey_hh_weight)

w_ni <- lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
             clerks + tech_aspros,
           data = hh_occ,
           weights =  survey_hh_weight)

# Impact of urban_rural
w_ni_ur_dum <- lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
                    clerks + tech_aspros + urban_rural,
                  data = hh_occ,
                  weights =  survey_hh_weight)

w_ni_ur_int <- lm(hh_income ~ 0 + not_emp*urban_rural + ag_othlowsk*urban_rural + p65*urban_rural +
                    service_shop*urban_rural + off_mgr_pros*urban_rural +
                    clerks*urban_rural + tech_aspros*urban_rural,
                  data = hh_occ,
                  weights =  survey_hh_weight)

w_ni_r <- lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
               clerks + tech_aspros, 
             data = filter(hh_occ, urban_rural == "rural"),
             weights =  survey_hh_weight)

w_ni_u <- lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
               clerks + tech_aspros,
             data = filter(hh_occ, urban_rural == "urban"),
             weights =  survey_hh_weight)


# Inspect
# plot(w_ni)
screenreg(list(w_ni, w_ni_ur_dum, w_ni_ur_int, w_ni_r, w_ni_u))
dwplot(list(w_ni, w_ni_r, w_ni_u),
       dot_args = list(size = 2),
       vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  scale_color_discrete(labels = c("w_ni", "w_ni_r", "w_ni_u")) 
tidy(w_ni, conf.int = .95)


# ADM1 -----------------------------------------------------------------------------------
# For now we stick to the national estimations as the regional values for some adms are not
# consistent, probably because some occupations are rare. Need to investigate and potentially
# merge several small areas, such as SNNP

adm1_reg <- hh_occ %>%
  nest(data = -adm1_name) %>%
  mutate(
    fit = map(data, ~ lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
                           clerks + tech_aspros, weights =  survey_hh_weight, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

coef <- adm1_reg %>%
  unnest(tidied) %>%
  dplyr::select(-data, -fit, -glanced, -augmented) %>%
  rename(model = adm1_name)

fit <- adm1_reg %>%
  unnest(glanced) %>%
  dplyr::select(-data, -fit, -tidied, -augmented)


library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
hh_occ %>%
  split(.$adm1_name) %>%
  map(~ lm(hh_income ~ 0 + not_emp + ag_othlowsk + p65 + service_shop + off_mgr_pros +
             clerks + tech_aspros, weights =  survey_hh_weight, data = .)) %>%
  dwplot(dot_args = list(size = 2),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  scale_color_manual(values = getPalette(11))


