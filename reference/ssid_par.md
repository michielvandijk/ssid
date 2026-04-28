# Sets `ssid` parameters

`ssid_par` sets all required parameters to run `ssid`, including core
model folders, country code and year.

## Usage

``` r
ssid_par(
  model_path = NULL,
  db_path = NULL,
  iso3c = NULL,
  adm_level = NULL,
  micro_year = NULL,
  start_year = NULL,
  end_year = NULL
)
```

## Arguments

- model_path:

  character. Folder where the model files are stored. Note that R uses
  forward slash or double backslash to separate folder names.

- db_path:

  character. Folder where the input database is stored.

- iso3c:

  character. Three letter ISO 3166-1 alpha-3 country code, also referred
  to as iso3c. A list of country codes can be found in
  [Wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3).

- adm_level:

  numeric. Administrative unit level at which the model is run. This
  needs to correspond with the information in the shapefile of the
  administrative units, which is needed as input.

- micro_year:

  numeric. Year for which the micro data (household survey) is
  available.

- start_year:

  numeric. Start year of the simulation period.

- end_year:

  numeric. End year of the simulation period.

## Value

ssid_par object

## Details

`ssid_par` creates an object of class `ssid_par`, which bundles all
required `ssid` parameters set by the user: model and database folder,
three digit country code, country name and continent. The coordinate
reference system is automatically set to WGS84 (epsg:4326).

`countrycode::countrycode()` is used to determine the full country name,
three digit country code and continent on the basis of the alpha-3
country code. This information is required to extract country specific
information from several datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
ssid_par(model_path = "C:/temp/ssid_eth", db_path = "C:/temp/ssid_db",
iso3c = "ETH", adm_level = 2, micro_year = 2018, proj_year = 2021, start_year = 2018, end_year = 2050)
} # }
```
