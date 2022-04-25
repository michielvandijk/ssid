#' Title
#'
#' @param r
#' @param adm_file
#'
#' @return
#' @export
#'
#' @examples
ag_adm <- function(r, adm_file){
  cat(basename(r), "\n")
  df <- adm_file %>%
    st_drop_geometry() %>%
    mutate(
      sex = strsplit(basename(r), '[_]')[[1]][1],
      age = strsplit(basename(r), '[_]')[[1]][2],
      year = as.integer(strsplit(basename(r), '[_]')[[1]][3]),
      value = exact_extract(rast(r), adm, fun = "sum"))
  return(df)
}
