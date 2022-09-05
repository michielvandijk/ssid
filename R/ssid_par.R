#'@title
#'Sets `ssid` parameters
#'
#'@description
#'`ssid_par` sets all required parameters to run `ssid`, including core model
#'folders, country code and year.
#'
#'@details
#'`ssid_par` creates an object of class `ssid_par`, which bundles all required
#'`ssid` parameters set by the user: model folder, three digit country code, country name and
#'continent. The coordinate reference system is automatically set to WGS84
#'(epsg:4326).
#'
#'[countrycode::countrycode()] is used to determine the full country
#'name, three digit country code and continent on
#'the basis of the alpha-3 country code. This information is required to extract
#'country specific information from several datasets.
#'
#'@param model_path character string with the main model folder. Note that R uses
#'  forward slash or double backslash to separate folder names.
#'@param iso3c character string with the three letter ISO 3166-1 alpha-3 country
#'  code, also referred to as iso3c. A list of country codes can be found in
#'  [Wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3).
#'@param year numeric with the reference year of the model.
#'
#'@return ssid_par object
#'
#'@examples
#'\dontrun{
#'mapspamc_par(model_path = "C:/temp/ssid_eth",
#'iso3c = "ETH", year = 2018)
#'}
#'@export
ssid_par <-
    function(model_path = NULL,
             iso3c = NULL,
             year = NULL) {

      param <- list(
            iso3c = ifelse(!is.null(iso3c), toupper(iso3c), NA_character_),
            country = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "country.name"), NA_character_),
            iso3n = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "iso3n"), NA_character_),
            continent = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "continent"), NA_character_),
            year = year,
            model_path = model_path,
            crs = "epsg:4326")
        class(param) <- "ssid_par"
        validate_ssid_par(param)
        return(param)
    }

# Function to validate ssid_par class
validate_ssid_par <- function(param) {
  stopifnot(inherits(param, "ssid_par"))
  if (is.null(param$model_path))
    stop("model_path is not defined",
         call. = FALSE)
  if (is.na(param$iso3c)) {
    stop("iso3c not defined",
         call. = FALSE)
  } else {
    if(!grepl("^[A-Z]{3}$", param$iso3c)) {
      stop("iso3c is not a three letter character",
           call. = FALSE)
    }
  }
  if (is.null(param$year)) {
    stop("year is not defined",
         call. = FALSE)
  } else {
    if(!is.numeric(param$year)) {
      stop("year is not a value",
           call. = FALSE)
    } else {
      if(param$year < 2000 | param$year > 2030) {
        message("year seems to have an unrealistic value")
      }
    }
  }
}


