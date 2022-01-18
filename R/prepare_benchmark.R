#' Title
#'
#' @param hh
#' @param u_r
#' @param a_s
#' @param occupation
#'
#' @return
#' @export
#'
#' @examples
prepare_benchmark <- function(hh, u_r, a_s, occupation){

  # Extract hh_number for all zones
  hh_number <- hh %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, n = value) %>%
    distinct()

  # Extract per targets for all zones
  per_urban_rural <- u_r %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, urban_rural, scenario, year, value) %>%
    distinct() %>%
    pivot_wider(names_from = urban_rural, values_from = value, values_fill = 0)

  per_age <- a_s %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, age, value) %>%
    group_by(scenario, year, target_zone, region, age) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = age, values_from = value, values_fill = 0)

  per_sex <- a_s %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, sex, value) %>%
    group_by(scenario, year, target_zone, region, sex) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = sex, values_from = value, values_fill = 0)

  per_occ <- occ %>%
    dplyr::select(target_zone = adm2_code, region = adm1_name, scenario, year, variable, value) %>%
    group_by(scenario, year, target_zone, region, variable) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    distinct() %>%
    pivot_wider(names_from = variable, values_from = value, values_fill = 0)

  hh_bm <- list(hh_number = hh_number)
  per_bm <- list(
    per_urban_rural = per_urban_rural,
    per_age = per_age,
    per_sex = per_sex,
    per_occ = per_occ)
  bm <- list(hh_bm = hh_bm, per_bm = per_bm)
  return(bm)
}
