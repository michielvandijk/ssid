#' Extract figures with with weight distribution
#'
#' @param sim Object with spatial simulation results.
#' @param ssp_y Vector with ssp-year combinations for which simulation results are stored in `sim`.
#' @param reg_tz Vector with regions for which simulation results are stored in `sim`.
#'
#' @return
#' @export
#'
#' @examples
extract_weight_plots <- function(sim, ssp_y, reg_tz) {
  for(i in ssp_y){
    for(j in reg_tz){
      n <- paste(i, j, sep = " ")
      cat(n,"\n")
      print(sim[[i]][[j]]$weight_dist +
              labs(title = n))
    }
  }
}
