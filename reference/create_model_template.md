# Creates `ssid` model template

`create_model_template` creates a folder structure with template R
scripts to implement the various steps to run `ssid`.

## Usage

``` r
create_model_template(template_path = NULL)
```

## Arguments

- template_path:

  folder where template scripts will be copied.

## Details

`create_model template()` creates several folders in the
`template_path`, set by the user. The folders contain template R scripts
to implement `ssid`.

To run the template scripts, they have to be copied into an RStudio
project. We recommend to create such a project first and then use
`create_model_template()` to add the scripts to the RStudio project.
Note that `create_model_template()` only copies script files if they do
not exist yet. In this way, the user cannot accidentally replace already
modified scripts.

## Examples

``` r
if (FALSE) { # \dontrun{
create_model_template("c:/temp")
} # }
```
