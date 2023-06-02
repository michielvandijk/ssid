# ========================================================================================
# Project:  ssid
# Subject:  Reweighing of seed
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("inst/model_setup_for_debug.r"))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Seed
hh_db <- readRDS(file.path(param$model_path, glue("seed/seed_hh_db_{param$iso3c}.rds")))
per_db <- readRDS(file.path(param$model_path, glue("seed/seed_per_db_{param$iso3c}.rds")))

# Adm_list
adm_list <- readRDS(file.path(param$model_path, glue("adm/adm_list_{param$iso3c}.rds")))

# Subnat urban-rural projections
subnat_urban_rural_proj <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_urban_rural_proj_{param$iso3c}.rds")))
subnat_urban_rural_proj_m15 <- readRDS(file.path(param$model_path,
                                                 glue("benchmark/subnat_urban_rural_proj_m15_{param$iso3c}.rds")))

# Subnat age_sex projections
subnat_age_sex_proj <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_age_sex_proj_{param$iso3c}.rds")))
subnat_age_sex_proj_m15 <- readRDS(file.path(param$model_path,
                                             glue("benchmark/subnat_age_sex_proj_m15_{param$iso3c}.rds")))

# Subnat occupation projections
subnat_occ_proj <- readRDS(file.path(param$model_path,
                                     glue("benchmark/subnat_occupation_proj_{param$iso3c}.rds")))
subnat_occ_proj_m15 <- readRDS(file.path(param$model_path,
                                         glue("benchmark/subnat_occupation_proj_m15_{param$iso3c}.rds")))

# Subnat household projections
subnat_hh_proj <- readRDS(file.path(param$model_path,
                                    glue("benchmark/subnat_hh_proj_{param$iso3c}.rds")))
subnat_hh_proj_m15 <- readRDS(file.path(param$model_path,
                                        glue("benchmark/subnat_hh_proj_m15_{param$iso3c}.rds")))



# ========================================================================================
# PREPARE -------------------------------------------------------------------------------
# ========================================================================================

# Create region - target zone id
adm_list <- adm_list %>%
  dplyr::select(target_zone = adm2_code, region = adm1_name) %>%
  distinct() %>%
  mutate(reg_tz = paste(region, target_zone, sep = "-x-"))


# ========================================================================================
# SIMULATION FOR PROBLEMATIC REGION ------------------------------------------------------
# ========================================================================================

# SEED ------------------------------------------------------------------------------------

# We exclude any families that only consist of m15 members. If not, ipu cannot solved as
# there is no matching data in per seed. In this case n = 1
hh_seed_no_by <- hh_db %>%
  filter(seed_hh_size != seed_hh_size_m15) %>%
  rename(region = adm1_name) %>%
  dplyr::select(region, id = hh_id) %>%
  mutate(weight = 1,
         hh_number = "n")

# We exclude the m15 category and distribute the number of children post process
per_seed_no_by <- per_db %>%
  rename(region = adm1_name) %>%
  filter(age != "m15") %>%
  dplyr::select(region, id = hh_id, sex, age = age, urban_rural, occupation) %>%
  droplevels()

# BENCHMARK -------------------------------------------------------------------------------

# We use the projections that exclude the m15 class
bm_no_by <- prepare_benchmark(subnat_hh_proj_m15, subnat_urban_rural_proj_m15,
                              subnat_age_sex_proj_m15, subnat_occ_proj_m15)


# SIMULATION ------------------------------------------------------------------------------

ssp_y <- expand.grid(ssp = c("ssp1", "ssp2", "ssp3"),
                     y = c(2020, 2030, 2040, 2050), stringsAsFactors = FALSE) %>%
  mutate(ssp_y = paste(ssp, y, sep = "_"))

tic()
sim_no_by <- map(ssp_y$ssp_y[1], reweigh, adm_list$reg_tz[1], hh_seed_no_by, per_seed_no_by, bm_no_by, param,
                 verbose = FALSE, reg_sample = FALSE, max_iter = 500, max_ratio = 20, min_ratio = 0.01, relative_gap = 0.05,
                 absolute_diff = 10, parallel = TRUE, output = temp_path)
names(sim_no_by) <- ssp_y$ssp_y
toc()



# ========================================================================================
# DEBUG ----------------------------------------------------------------------------------
# ========================================================================================

# Set input parameters
ssp_y <- ssp_y$ssp_y[1]
reg_tz <- adm_list$reg_tz[1]
hh_s <- hh_seed_no_by
per_s <- per_seed_no_by
bm <- bm_no_by
reg_sample = FALSE
verbose = FALSE
max_iter = 500
max_ratio = 20
min_ratio = 0.2
relative_gap = 0.05
absolute_diff = 10
parallel = FALSE
output = NULL

# Loop over year and ssp combinations
reweigh <- function(ssp_y, reg_tz, hh_s, per_s, bm, param, reg_sample = FALSE,
                    verbose = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_diff = 10, parallel = TRUE,
                    output = NULL){
  cat(ssp_y)
  ssp <- str_split(ssp_y, pattern = "_")[[1]][1]
  y <- str_split(ssp_y, pattern = "_")[[1]][2]
  bm <- select_benchmark(ssp, y, bm)
  # if (parallel) {
  #   plan(multisession, workers = availableCores())
  # } else {
  #   plan(sequential)
  # }
#  with_progress({
#    p <- progressor(steps = length(reg_tz))
    sim <- reg_tz %>%
      set_names() %>%
      #furrr::future_map(
      purrr::map(
        ipf_seed,
        hh_s,
        per_s,
#        p = p,
        bm,
        reg_sample = reg_sample,
        verbose = verbose,
        max_iter = max_iter,
        max_ratio = max_ratio,
        min_ratio = min_ratio,
        relative_gap = relative_gap,
        absolute_diff = absolute_diff
      )
#  })
  plan(sequential)
  if (!is.null(output)) {
    dir.create(output, showWarnings = FALSE)
    if(reg_sample) {
      file_name <- glue::glue("{ssp_y}_adm1_sample_{param$iso3c}.rds")
    } else {
      file_name <- glue::glue("{ssp_y}_full_sample_{param$iso3c}.rds")
    }
    saveRDS(sim, file.path(output, file_name))
  }
  return(sim)
}



ipf_seed <- function(reg_tz, hh_s, per_s, bm, p, reg_sample = FALSE, verbose = FALSE,
                     max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                     relative_gap = 0.01, absolute_diff = 10){

  # To display progress bar in combination with furrr and progressr
  #p()

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

  sim <- ipfr::ipu(hh_s_r, hh_t_r, per_s_r, per_t_r, max_iterations = max_iter, verbose = verbose,
                   max_ratio = max_ratio, min_ratio = min_ratio, relative_gap = relative_gap, absolute_diff = absolute_diff)


  summary(hh_s_r)
  summary(hh_t_r[[1]])
  summary(per_s_r)
  summary(per_t_r)
  return(sim)
}

tar <- hh_t_r[[1]]
function(tar, reg, tz){
  var <- names(tar)[!names(tar) %in% c("target_zone", "region", "scenario", "year")]
  out <- tar[tar$region == reg & tar$target_zone == tz, var]
  return(out)
}

primary_seed <- hh_s_r
primary_targets <- hh_t_r
secondary_seed <- per_s_r
secondary_targets <- per_t_r
primary_id = "id"
secondary_importance = 1

?ipfr::ipu
function (primary_seed, primary_targets, secondary_seed = NULL,
          secondary_targets = NULL, primary_id = "id", secondary_importance = 1,
          relative_gap = 0.01, max_iterations = 100, absolute_diff = 10,
          weight_floor = 0.00001, verbose = FALSE, max_ratio = 10000,
          min_ratio = 0.0001)
{
  if (xor(!is.null(secondary_seed), !is.null(secondary_targets))) {
    stop("You provided either secondary_seed or secondary_targets, but not both.")
  }
  if (secondary_importance > 1 | secondary_importance < 0) {
    stop("`secondary_importance` argument must be between 0 and 1")
  }
  if (!is.null(secondary_seed)) {
    result <- ipfr:::check_tables(primary_seed, primary_targets,
                           primary_id = primary_id, secondary_seed, secondary_targets)
  }
  else {
    result <- check_tables(primary_seed, primary_targets,
                           primary_id = primary_id)
  }
  primary_seed <- result[[1]]
  primary_targets <- result[[2]]
  secondary_seed <- result[[3]]
  secondary_targets <- result[[4]]
  primary_targets <- scale_targets(primary_targets, verbose)
  if (!is.null(secondary_seed)) {
    secondary_targets <- scale_targets(secondary_targets,
                                       verbose)
  }
  if (secondary_importance != 1 & !is.null(secondary_seed)) {
    if (verbose) {
      message("Balancing secondary targets to primary")
    }
    secondary_targets_mod <- balance_secondary_targets(primary_targets,
                                                       primary_seed, secondary_targets, secondary_seed,
                                                       secondary_importance, primary_id)
  }
  else {
    secondary_targets_mod <- secondary_targets
  }
  geo_equiv <- primary_seed %>% dplyr::select(dplyr::starts_with("geo_"),
                                              primary_id, "weight")
  marginal_columns <- names(primary_targets)
  primary_seed_mod <- process_seed_table(primary_seed, primary_id,
                                         marginal_columns)
  if (!is.null(secondary_seed)) {
    marginal_columns <- names(secondary_targets_mod)
    secondary_seed_mod <- process_seed_table(secondary_seed,
                                             primary_id, marginal_columns) %>% dplyr::group_by(!!as.name(primary_id)) %>%
      dplyr::summarize_all(.funs = sum)
    seed <- primary_seed_mod %>% dplyr::left_join(secondary_seed_mod,
                                                  by = primary_id)
  }
  else {
    seed <- primary_seed_mod
  }
  seed <- seed %>% dplyr::left_join(geo_equiv, by = primary_id)
  geo_pos <- grep("geo_", colnames(seed))
  id_pos <- grep(primary_id, colnames(seed))
  weight_pos <- grep("weight", colnames(seed))
  seed_attribute_cols <- colnames(seed)[-c(geo_pos, id_pos,
                                           weight_pos)]
  if (!is.null(secondary_seed)) {
    targets <- c(primary_targets, secondary_targets_mod)
  }
  else {
    targets <- primary_targets
  }
  for (name in names(targets)) {
    temp <- targets[[name]] %>% tidyr::gather(key = "key",
                                              value = "target", -dplyr::starts_with("geo_")) %>%
      dplyr::mutate(key = paste0(!!name, ".", key, ".target")) %>%
      tidyr::spread(key = key, value = target)
    pos <- grep("geo_", colnames(temp))
    geo_colname <- colnames(temp)[pos]
    seed <- seed %>% dplyr::left_join(temp, by = geo_colname)
  }
  pos <- grep("geo_", colnames(targets[[1]]))
  geo_colname <- colnames(targets[[1]])[pos]
  recs_by_geo <- seed %>% dplyr::group_by(!!as.name(geo_colname)) %>%
    dplyr::summarize(count = n())
  weight_scale <- targets[[1]] %>% tidyr::gather(key = category,
                                                 value = total, -!!as.name(geo_colname)) %>% dplyr::group_by(!!as.name(geo_colname)) %>%
    dplyr::summarize(total = sum(total)) %>% dplyr::left_join(recs_by_geo,
                                                              by = geo_colname) %>% dplyr::mutate(avg_weight = total/count,
                                                                                                  min_weight = (!!min_ratio) * avg_weight, max_weight = (!!max_ratio) *
                                                                                                    avg_weight)
  seed <- seed %>% dplyr::left_join(weight_scale, by = geo_colname)
  iter <- 1
  converged <- FALSE
  while (!converged & iter <= max_iterations) {
    for (seed_attribute in seed_attribute_cols) {
      target_tbl_name <- strsplit(seed_attribute, ".",
                                  fixed = TRUE)[[1]][1]
      target_name <- paste0(seed_attribute, ".", "target")
      target_tbl <- targets[[target_tbl_name]]
      pos <- grep("geo_", colnames(target_tbl))
      geo_colname <- colnames(target_tbl)[pos]
      seed <- seed %>% dplyr::mutate(geo = !!as.name(geo_colname),
                                     attr = !!as.name(seed_attribute), target = !!as.name(target_name)) %>%
        dplyr::group_by(geo) %>% dplyr::mutate(total_weight = sum(attr *
                                                                    weight), factor = ifelse(attr > 0, target/total_weight,
                                                                                             1), weight = weight * factor, weight = pmax(weight,
                                                                                                                                         weight_floor), weight = ifelse(attr > 0 & target >
                                                                                                                                                                          0, pmax(min_weight, weight), weight), weight = ifelse(attr >
                                                                                                                                                                                                                                  0 & target > 0, pmin(max_weight, weight), weight)) %>%
        dplyr::ungroup() %>% dplyr::select(-geo, -attr,
                                           -target, -factor)
    }
    saved_diff_tbl <- NULL
    pct_diff <- 0
    for (seed_attribute in seed_attribute_cols) {
      target_tbl_name <- strsplit(seed_attribute, ".",
                                  fixed = TRUE)[[1]][1]
      target_name <- paste0(seed_attribute, ".", "target")
      target_tbl <- targets[[target_tbl_name]]
      pos <- grep("geo_", colnames(target_tbl))
      geo_colname <- colnames(target_tbl)[pos]
      diff_tbl <- seed %>% dplyr::filter((!!as.name(seed_attribute)) >
                                           0) %>% dplyr::select(geo = !!geo_colname, primary_id,
                                                                attr = !!seed_attribute, weight, target = !!target_name) %>%
        dplyr::group_by(geo) %>% dplyr::mutate(total_weight = sum(attr *
                                                                    weight), diff = total_weight - target, abs_diff = abs(diff),
                                               pct_diff = diff/(target + 0.0000001)) %>% dplyr::filter(abs_diff >
                                                                                                         absolute_diff) %>% dplyr::slice(1) %>% dplyr::ungroup()
      if (nrow(diff_tbl) > 0) {
        if (max(abs(diff_tbl$pct_diff)) > pct_diff) {
          pct_diff <- max(abs(diff_tbl$pct_diff))
          saved_diff_tbl <- diff_tbl
          saved_category <- seed_attribute
          saved_geo <- geo_colname
        }
      }
    }
    if (iter > 1) {
      rmse <- mlr::measureRMSE(prev_weights, seed$weight)
      pct_rmse <- rmse/mean(prev_weights) * 100
      converged <- ifelse(pct_rmse <= relative_gap, TRUE,
                          FALSE)
      if (verbose) {
        cat("\r Finished iteration ", iter, ". %RMSE = ",
            pct_rmse)
      }
    }
    prev_weights <- seed$weight
    iter <- iter + 1
  }
  if (verbose) {
    message(ifelse(converged, "\nIPU converged", "\nIPU did not converge"))
    if (is.null(saved_diff_tbl)) {
      message("All targets matched within the absolute_diff of ",
              absolute_diff)
    }
    else {
      message("Worst marginal stats:")
      position <- which(abs(saved_diff_tbl$pct_diff) ==
                          pct_diff)[1]
      message("Category: ", saved_category)
      message(saved_geo, ": ", saved_diff_tbl$geo[position])
      message("Worst % Diff: ", round(saved_diff_tbl$pct_diff[position] *
                                        100, 2), "%")
      message("Difference: ", round(saved_diff_tbl$diff[position],
                                    2))
    }
    utils::flush.console()
  }
  primary_seed$weight <- seed$weight
  primary_seed$avg_weight <- seed$avg_weight
  primary_seed$weight_factor <- primary_seed$weight/primary_seed$avg_weight
  primary_seed <- primary_seed %>% mutate(weight = ifelse(avg_weight ==
                                                            0, 0, weight), weight_factor = ifelse(avg_weight == 0,
                                                                                                  0, weight_factor))
  result <- list()
  result$weight_tbl <- primary_seed
  result$weight_tbl$geo_all <- NULL
  result$weight_dist <- ggplot2::ggplot(data = primary_seed,
                                        ggplot2::aes(primary_seed$weight_factor)) + ggplot2::geom_histogram(bins = 10,
                                                                                                            fill = "darkblue", color = "gray") + ggplot2::labs(x = "Weight Ratio = Weight / Average Weight",
                                                                                                                                                               y = "Count of Seed Records")
  primary_comp <- compare_results(primary_seed, primary_targets)
  result$primary_comp <- primary_comp
  if (!is.null(secondary_seed)) {
    pos <- grep("geo_", colnames(primary_seed))
    geo_cols <- colnames(primary_seed)[pos]
    seed <- secondary_seed %>% dplyr::left_join(primary_seed %>%
                                                  dplyr::select(dplyr::one_of(geo_cols), primary_id,
                                                                weight), by = primary_id)
    secondary_comp <- compare_results(seed, secondary_targets)
    result$secondary_comp <- secondary_comp
  }
  stats <- list()
  stats[["stats_sum"]] <- data.frame(iterations = iter, pct_rmse = pct_rmse,
                                     converged = converged)
  if (!is.null(saved_diff_tbl)) {
    position <- which(abs(saved_diff_tbl$pct_diff) == pct_diff)[1]
    stats[["stats_sum"]] <- data.frame(iterations = iter,
                                       pct_rmse = pct_rmse, converged = converged, worst_marginal_stats_category = saved_category,
                                       worst_per_diff = round(saved_diff_tbl$pct_diff[position] *
                                                                100, 2), abs_difference = round(saved_diff_tbl$diff[position],
                                                                                                2))
    stats[["diff_tbl"]] <- saved_diff_tbl
    stats[["geo_all"]] <- saved_geo
  }
  result$stats <- stats
  return(result)
}



#ipfr:::check_tables
function (primary_seed, primary_targets, secondary_seed = NULL,
          secondary_targets = NULL, primary_id)
{
  if (xor(!is.null(secondary_seed), !is.null(secondary_targets))) {
    stop("You provided either secondary_seed or secondary_targets, but not both.")
  }
  if (any(is.na(unlist(primary_seed)))) {
    stop("primary_seed table contains NAs")
  }
  if (any(is.na(unlist(primary_targets)))) {
    stop("primary_targets table contains NAs")
  }
  if (!"weight" %in% colnames(primary_seed)) {
    primary_seed$weight <- 1
  }
  secondary_seed_exists <- !is.null(secondary_seed)
  id_field_exists <- primary_id %in% colnames(primary_seed)
  if (!id_field_exists) {
    if (secondary_seed_exists) {
      stop("The primary seed table does not have field, '",
           primary_id, "'.")
    }
    else {
      primary_seed[primary_id] <- seq(1, nrow(primary_seed))
    }
  }
  unique_ids <- unique(primary_seed[[primary_id]])
  if (length(unique_ids) != nrow(primary_seed)) {
    stop("The primary seed's ", primary_id, " field has duplicate values.")
  }
  for (name in names(primary_targets)) {
    tbl <- primary_targets[[name]]
    result <- ipfr:::check_geo_fields(primary_seed, tbl, name)
    primary_seed <- result[[1]]
    primary_targets[[name]] <- result[[2]]
    tbl <- result[[2]]
    pos <- grep("geo_", colnames(tbl))
    geo_colname <- colnames(tbl)[pos]
    ipfr:::check_missing_categories(primary_seed, tbl, name, geo_colname)
  }
  if (secondary_seed_exists) {
    if (any(is.na(unlist(secondary_seed)))) {
      stop("secondary_seed table contains NAs")
    }
    if (any(is.na(unlist(secondary_targets)))) {
      stop("secondary_targets table contains NAs")
    }
    if (!primary_id %in% colnames(secondary_seed)) {
      stop("The primary seed table does not have field '",
           primary_id, "'.")
    }
    check <- grepl("geo_", colnames(secondary_seed))
    if (any(check)) {
      stop("Do not include geo fields in the secondary_seed table (primary_seed only).")
    }
    for (name in names(secondary_targets)) {
      print(name)
      tbl <- secondary_targets[[name]]
      result <- ipfr:::check_geo_fields(secondary_seed, tbl, name)
      secondary_seed <- result[[1]]
      if ("geo_all" %in% colnames(secondary_seed)) {
        secondary_seed$geo_all <- NULL
        primary_seed$geo_all <- 1
      }
      secondary_targets[[name]] <- result[[2]]
      tbl <- result[[2]]
      pos <- grep("geo_", colnames(tbl))
      geo_colname <- colnames(tbl)[pos]
      temp_seed <- secondary_seed %>% dplyr::left_join(primary_seed %>%
                                                         dplyr::select(primary_id, geo_colname), by = primary_id)
      ipfr:::check_missing_categories(temp_seed, tbl, name, geo_colname)
    }
  }
  return(list(primary_seed, primary_targets, secondary_seed,
              secondary_targets))
}


#ipfr:::check_missing_categories
seed <- temp_seed
# NA VALUES in geo_colname
nrow(seed[is.na(seed$geo_all),])
function (seed, target, target_name, geo_colname)
{
  for (geo in unique(unlist(seed[, geo_colname]))) {
    non_zero_targets <- target[target[[geo_colname]] == geo,
                               colSums(target[target[[geo_colname]] == geo, ]) >
                                 0]
    col_names <- colnames(non_zero_targets)
    col_names <- type.convert(col_names[!col_names == geo_colname],
                              as.is = TRUE)
    test <- match(col_names, seed[[target_name]][seed[, geo_colname] ==
                                                   geo])
    if (any(is.na(test))) {
      prob_cat <- col_names[which(is.na(test))]
      stop("Marginal ", target_name, " category ", paste(prob_cat,
                                                         collapse = ", "), " missing from ", geo_colname,
           " ", geo, " in the seed table with a target greater than zero.")
    }
  }
}
