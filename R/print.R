#'@export
print.ssid_par <- function(x, ...) {
    cat("country name: ", x$country, "\n")
    cat("iso3n: ", x$iso3n, "\n")
    cat("iso3c: ", x$iso3c, "\n")
    cat("continent: ", x$continent, "\n")
    cat("year: ", x$year, "\n")
    cat("model path: ", x$model_path, "\n")
    cat("ssid database path: ", x$db_path, "\n")
    cat("crs: ", x$crs, "\n")
}

