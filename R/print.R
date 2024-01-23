#'@export
print.ssid_par <- function(x, ...) {
    cat("country: ", x$country, "\n")
    cat("iso3n: ", x$iso3n, "\n")
    cat("iso3c: ", x$iso3c, "\n")
    cat("continent: ", x$continent, "\n")
    cat("adm_level: ", x$adm_level, "\n")
    cat("micro_year: ", x$micro_year, "\n")
    cat("start_year: ", x$start_year, "\n")
    cat("end_year: ", x$end_year, "\n")
    cat("model_path: ", x$model_path, "\n")
    cat("db_path: ", x$db_path, "\n")
    cat("crs: ", x$crs, "\n")
}

