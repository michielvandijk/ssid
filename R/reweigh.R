#' Title
#'
#' @param ssp_y
#' @param reg_tz
#' @param hh_s
#' @param per_s
#' @param bm
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
reweigh <- function(ssp_y, reg_tz, hh_s, per_s, bm,
                    verbose = FALSE, max_iter = 300, max_ratio = 5, min_ratio = 0.2,
                    relative_gap = 0.01, absolute_gap = 10){
  cat(ssp_y)
  ssp <- str_split(ssp_y, pattern = "_")[[1]][1]
  y <- str_split(ssp_y, pattern = "_")[[1]][2]
  plan(multisession, workers = max(0, availableCores()-4))
  with_progress({
    p <- progressor(steps = length(reg_tz))
    sim <- reg_tz %>%
      set_names %>%
      future_map(ipf_seed, hh_s, per_s, select_benchmark(ssp, y, bm), p = p,
                 verbose = verbose, max_iter = max_iter, max_ratio = max_ratio,
                 min_ratio = min_ratio, relative_gap = relative_gap)
  })
  plan(sequential)
  return(sim)
}
