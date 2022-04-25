# Function to clip country map
#' Title
#'
#' @param clip_file
#' @param iso3c
#' @param input_file
#'
#' @return
#' @export
#'
#' @examples
clip_country <- function(clip_file, iso3c, input_file){
  sex <- str_split(basename(input_file), pattern = "_")[[1]][[2]]
  age <- str_split(basename(input_file), pattern = "_")[[1]][[3]]
  year <- str_split(basename(input_file), pattern = "_")[[1]][[4]]
  output_folder <- file.path(proc_path, glue("benchmark/worldpop_age_sex"))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  file_name <- glue("{sex}_{age}_{year}_{iso3c}.tif")
  output_file <- glue("{output_folder}/{file_name}")
  cat(basename(output_file), "\n")
  output_map <- gdalwarp(srcfile = input_file, dstfile = output_file,
                         cutline = clip_file, crop_to_cutline = T,
                         r = "near", overwrite = T)
  plot(rast(output_map), main = basename(output_file))
}
