#' Function to clip national maps from global WorldPop sex and age disaggregated population maps
#'
#' @param r Object of class SpatRaster, Raster or their path
#' @param adm object of class sf with the location of subnational units
#'
#' @return
#' @export
#'
#' @examples
clip_country <- function(r, adm){
  r <- terra::rast(r)
  poly <- terra::vect(adm)
  r <- terra::crop(r, poly, mask = TRUE)
}
