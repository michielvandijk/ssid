#' Extract weights and diagnostics from spatial simulation object
#'
#' @param df Object with spatial simulation results nested by ssp-year and
#'   region.
#' @param var Type of output that needs to be extracted. Options are: stats,
#'   weight_tbl, primary_comp, secondary_comp
#'
#' @return
#' @export
#'
#' @examples
extract_output <- function(df, var){
  out <- map_df(df, function(x) map_df(x, var, .id = "reg_tz"), .id = "ssp_y")
  return(out)
}

