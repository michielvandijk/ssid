select_benchmark <- function(ssp, y, bm){
  hh_number <- bm$hh_bm$hh_number %>%
    filter(year == y, scenario == ssp)
  per_urban_rural <- bm$per_bm$per_urban_rural %>%
    filter(year == y, scenario == ssp)
  per_age <- bm$per_bm$per_age %>%
    filter(year == y, scenario == ssp)
  per_sex <- bm$per_bm$per_sex %>%
    filter(year == y, scenario == ssp)
  per_occ <- bm$per_bm$per_occ %>%
    filter(year == y, scenario == ssp)
  hh_bm <- list(hh_number = hh_number)
  per_bm <- list(
    per_urban_rural = per_urban_rural,
    per_age = per_age,
    per_sex = per_sex,
    per_occ = per_occ)
  bm <- list(hh_bm = hh_bm, per_bm = per_bm)
  return(bm)
}


create_weights_pdf <- function(reg_tz, ss){
  cat(reg_tz, "\n")
  nms <- names(ss)
  h <- ss[[reg_tz]]$weight_dist
  print(h + labs(title = reg_tz))
}
