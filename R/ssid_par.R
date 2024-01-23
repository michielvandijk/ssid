#'@title
#'Sets `ssid` parameters
#'
#'@description
#'`ssid_par` sets all required parameters to run `ssid`, including core model
#'folders, country code and year.
#'
#'@details
#'`ssid_par` creates an object of class `ssid_par`, which bundles all required
#'`ssid` parameters set by the user: model and database folder, three digit country code, country name and
#'continent. The coordinate reference system is automatically set to WGS84
#'(epsg:4326).
#'
#'[countrycode::countrycode()] is used to determine the full country
#'name, three digit country code and continent on
#'the basis of the alpha-3 country code. This information is required to extract
#'country specific information from several datasets.
#'
#'@param model_path character. Folder where the model files are stored. Note that R uses
#'  forward slash or double backslash to separate folder names.
#'@param db_path character. Folder where the input database is stored.
#'@param iso3c character. Three letter ISO 3166-1 alpha-3 country
#'  code, also referred to as iso3c. A list of country codes can be found in
#'  [Wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3).
#'@param adm_level numeric. Administrative unit level at which the model is run. This needs to correspond with
#'the information in the shapefile of the administrative units, which is needed as input.
#'@param micro_year numeric. Year for which the micro data (household survey) is available.
#'@param start_year numeric. Start year of the simulation period.
#'@param end_year numeric. End year of the simulation period.
#'
#'@return ssid_par object
#'
#'@examples
#'\dontrun{
#'ssid_par(model_path = "C:/temp/ssid_eth", db_path = "C:/temp/ssid_db",
#'iso3c = "ETH", adm_level = 2, micro_year = 2018, proj_year = 2021, start_year = 2018, end_year = 2050)
#'}
#'@export
ssid_par <-
    function(model_path = NULL,
             db_path = NULL,
             iso3c = NULL,
             adm_level = NULL,
             micro_year = NULL,
             start_year = NULL,
             end_year = NULL) {

      param <- list(
            iso3c = ifelse(!is.null(iso3c), toupper(iso3c), NA_character_),
            country = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "country.name"), NA_character_),
            iso3n = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "iso3n"), NA_character_),
            continent = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "continent"), NA_character_),
            adm_level = adm_level,
            micro_year = micro_year,
            start_year = start_year,
            end_year = end_year,
            model_path = model_path,
            db_path = db_path,
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
  if (is.null(param$model_path))
    stop("db_path is not defined",
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
  if (is.null(param$adm_level)) {
    stop("adm_level is not defined",
         call. = FALSE)
  } else {
    if(!is.numeric(param$adm_level)) {
      stop("adm_level is not an integer",
           call. = FALSE)
    }
  }
  if (is.null(param$start_year)) {
    stop("start_year is not defined",
         call. = FALSE)
  } else {
    if(!is.numeric(param$start_year)) {
      stop("start_year is not an integer",
           call. = FALSE)
    } else {
      if(param$start_year < 2000 | param$start_year > 2030) {
        message("start_year seems to have an unrealistic value")
      }
    }
  }
  if (is.null(param$micro_year)) {
    stop("micro_year is not defined",
         call. = FALSE)
  } else {
    if(!is.numeric(param$micro_year)) {
      stop("micro_year is not an integer",
           call. = FALSE)
    }
  }
  if (is.null(param$end_year)) {
    stop("end_year is not defined",
         call. = FALSE)
  } else {
    if(!is.numeric(param$end_year)) {
      stop("end_year is not an integer",
           call. = FALSE)
    }
  }
}


