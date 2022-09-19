#' Title
#' Create figure with weight distribution
#'
#' @param region Region for which weights are extracted
#' @param sim_file Simulation file
#'
#' @return
#' @export
#'
#' @examples
create_weights_plot <- function(region, sim_file){
  cat(region, "\n")
  nms <- names(sim_file)
  h <- ss[[region]]$weight_dist
  print(h + labs(title = region))
}
