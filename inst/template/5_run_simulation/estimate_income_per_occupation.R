# ========================================================================================
# Project:  simFNS
# Subject:  Script to estimate income per occ
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

# seed
hh_db <- readRDS(file.path(param$model_path , glue("simulation/hh_db_{param$iso3c}.rds")))
per_db <- readRDS(file.path(param$model_path , glue("simulation/per_db_{param$iso3c}.rds")))


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
# REGRESSION -----------------------------------------------------------------------------
# ========================================================================================
# We follow Hallegate (2017) and we used weighted regression to relate family income
# (proxied by consumption) to occupation for each household member, excluding the intercept.

# weighted, no transformation
w_ni <- lm(hh_income ~ 0 + not_in_lf + ag_othlowsk + p65 + service_shop + off_mgr_pros +
          clerks + tech_aspros,
        data = hh_occ,
        weights =  survey_hh_weight)

# Impact of urban_rural
w_ni_r <- lm(hh_income ~ 0 + not_in_lf + ag_othlowsk + p65 + service_shop + off_mgr_pros +
               clerks + tech_aspros,
             data = filter(hh_occ, urban_rural == "rural"),
             weights =  survey_hh_weight)

w_ni_u <- lm(hh_income ~ 0 + not_in_lf + ag_othlowsk + p65 + service_shop + off_mgr_pros +
               clerks + tech_aspros,
             data = filter(hh_occ, urban_rural == "urban"),
             weights =  survey_hh_weight)

screenreg(list(w_ni, w_ni_r, w_ni_u))
dwplot(list(w_ni, w_ni_r, w_ni_u),
       dot_args = list(size = 2),
       vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  scale_color_discrete(labels = c(`Model 1` = "w_ni", `Model 2` = "w_ni_r", `Model 3` = "w_ni_u"))
tidy(w_ni, conf.int = .95)
tidy(w_ni_u, conf.int = .95)
tidy(w_ni_r, conf.int = .95)


# ========================================================================================
# PREPARE INCOME PER OCCUPATION DATA -----------------------------------------------------
# ========================================================================================

# We create separate income estimates for rural and urban

# Data.frame with income per person per class
per_income <- bind_rows(
  tidy(w_ni_r) %>%
    mutate(urban_rural = "rural"),
  tidy(w_ni_u) %>%
    mutate(urban_rural = "urban")) %>%
  dplyr::select(occupation = term, per_income_est = estimate, urban_rural)

# Data.frame with residual for each hh and income net of residual, i.e. sum of per_income
hh_income <- bind_rows(
  hh_db %>%
  filter(urban_rural == "rural") %>%
    dplyr::select(hh_id, hh_income, urban_rural) %>%
    mutate(residual = w_ni_r$residuals,
           hh_income_ex_resid = hh_income - residual),
  hh_db %>%
    filter(urban_rural == "urban") %>%
    dplyr::select(hh_id, hh_income, urban_rural) %>%
    mutate(residual = w_ni_u$residuals,
           hh_income_ex_resid = hh_income - residual)
)

# Check if income_ex_resid >0
filter(hh_income, hh_income_ex_resid <0)


#################### CHECK
# Set income to 0 hh with negative income
# This is not allowed as this will result in an infinite calculation.
# We must first calculate the incomes/food consumption of the hh, remove outliers
# and use that selection as the seed!
hh_income <- hh_income %>%
  mutate(hh_income_ex_resid = ifelse(hh_income_ex_resid < 0, 0, hh_income_ex_resid))

# Combine
est_income <- list(
  per_income = per_income,
  hh_income = hh_income)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

saveRDS(est_income, file.path(param$model_path, glue("simulation/estimated_income_{param$iso3c}.rds")))

