#'@export
print.ssid_par <- function(x, ...) {
    cat("country name: ", x$country, "\n")
    cat("iso3n: ", x$iso3n, "\n")
    cat("iso3c: ", x$iso3c, "\n")
    cat("continent: ", x$continent, "\n")
    cat("adm level: ", x$adm_level, "\n")
    cat("base year: ", x$base_year, "\n")
    cat("start year: ", x$start_year, "\n")
    cat("end year: ", x$end_year, "\n")
    cat("model path: ", x$model_path, "\n")
    cat("ssid database path: ", x$db_path, "\n")
    cat("crs: ", x$crs, "\n")
}

