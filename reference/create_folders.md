# Creates `ssid` folder structure

`create_folders` creates the folder structure that is needed to store
the processed data for `ssid`.

## Usage

``` r
create_folders(param = NULL)
```

## Arguments

- param:

  Object of type `ssid_par` that bundles all `ssid` parameters,
  including core model folders, alpha-3 country code and year and
  administrative unit level at which the model is solved and type of
  model.

## Details

`create_folders` creates two folders in the `model_path`, set by the
user: mappings and processed_data (including subfolders). It copies a
number of cvs files into the mappings folder, which contain several data
tables that are needed to run the model and, if needed can be adjusted
by the user.

## Examples

``` r
if (FALSE) { # \dontrun{
create_folders(param)
} # }
```
