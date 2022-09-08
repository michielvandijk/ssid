#' Aggregate raster files to adm level
#'
#' @description
#' Function to aggregate raster files to adm level.
#'
#' @param r Object of class SpatRaster or Raster
#' @param adm_file Object of class sf with location of subnational administrative units
#'
#' @return
#' @export
#'
#' @examples
aggregate2adm <- function(r, adm){
  df <- adm %>%
    sf::st_drop_geometry() %>%
    mutate(
      value = exactextractr::exact_extract(terra::rast(r), adm, fun = "sum"))
  return(df)
}
