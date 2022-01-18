#' ipf_seed
#'
#' @param reg_tz
#' @param hh_s
#' @param per_s
#' @param bm
#' @param p
#' @param reg_sample
#' @param verbose
#' @param max_iter
#' @param max_ratio
#' @param min_ratio
#' @param relative_gap
#' @param absolute_gap
#'
#' @return
#' @export
#'
#' @examples
ipf_seed <- function(reg_tz, hh_s, per_s, bm, p, reg_sample = FALSE, verbose = FALSE,
                     max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                     relative_gap = 0.01, absolute_gap = 10){

  # To display progress bar in combination with furrr and progressr
  p()

  prep_target <- function(tar, reg, tz){
    var <- names(tar)[!names(tar) %in% c("target_zone", "region", "scenario", "year")]
    out <- tar[tar$region == reg & tar$target_zone == tz, var]
    return(out)
  }

  reg <- strsplit(reg_tz, split = "-x-")[[1]][1]
  tz <- strsplit(reg_tz, split = "-x-")[[1]][2]
  cat("\n", tz, "in", reg, "\n")

  if(reg_sample){
    hh_s_r <- hh_s[hh_s$region == reg,]
    cat("Subnational seed sample used")
  } else {
    hh_s_r <- hh_s
  }

  hh_s_r <- hh_s_r[names(hh_s_r)[!names(hh_s_r) %in% c("region")]]

  hh_t_r <- list()
  hh_t_r[["hh_number"]] <- bm$hh_bm$hh_number
  hh_t_r <- lapply(hh_t_r, prep_target, reg, tz)

  if(reg_sample){
    per_s_r <- per_s[per_s$region == reg,]
  } else {
    per_s_r <- per_s
  }

  per_s_r <- per_s_r[names(per_s_r)[!names(per_s_r) %in% c("region")]]

  per_t_r <- list()
  per_t_r[["age"]] <- bm$per_bm$per_age
  per_t_r[["sex"]] <- bm$per_bm$per_sex
  per_t_r[["urban_rural"]] <- bm$per_bm$per_urban_rural
  per_t_r[["occupation"]] <- bm$per_bm$per_occ
  per_t_r <- lapply(per_t_r, prep_target, reg, tz)

  sim <- ipu(hh_s_r, hh_t_r, per_s_r, per_t_r, max_iterations = max_iter, verbose = verbose,
             max_ratio = max_ratio, min_ratio = min_ratio, relative_gap = relative_gap)
  return(sim)
}

