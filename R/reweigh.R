#' Title
#'
#' @param ssp_y
#' @param reg_tz
#' @param hh_s
#' @param per_s
#' @param bm
#' @param param
#' @param reg_sample
#' @param verbose
#' @param max_iter
#' @param max_ratio
#' @param min_ratio
#' @param relative_gap
#' @param absolute_diff
#' @param parallel If TRUE (the default), the reweighing algorithm is run in
#'   parallel for all regions. Otherwise, the sequential option is used (which
#'   in most cases takes much longer.
#' @param output A folder where to save the output for each ssp-year combination
#'   as specified in `ssp_y`. The default (NULL) is not to save any output.
#'
#' @return
#' @export
#'
#' @examples
reweigh <- function(ssp_y, reg_tz, hh_s, per_s, bm, param, reg_sample = FALSE,
                    verbose = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_diff = 10, parallel = TRUE,
                    output = NULL){
  cat(ssp_y)
  ssp <- str_split(ssp_y, pattern = "_")[[1]][1]
  y <- str_split(ssp_y, pattern = "_")[[1]][2]
  bm <- select_benchmark(ssp, y, bm)
  if (parallel) {
    plan(multisession, workers = availableCores())
  } else {
    plan(sequential)
  }
  with_progress({
    p <- progressor(steps = length(reg_tz))
    sim <- reg_tz %>%
      set_names() %>%
      furrr::future_map(
        ipf_seed,
        hh_s,
        per_s,
        p = p,
        bm,
        reg_sample = reg_sample,
        verbose = verbose,
        max_iter = max_iter,
        max_ratio = max_ratio,
        min_ratio = min_ratio,
        relative_gap = relative_gap,
        absolute_diff = absolute_diff
      )
  })
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
