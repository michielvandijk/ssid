# Helper function to process a seed table

Helper for
[`ipu()`](https://michielvandijk.github.io/ssid/reference/ipu.md).
Strips columns from seed table except for the primary id and marginal
column (as reflected in the targets tables). Also identifies factor
columns with one level and processes them before
[`mlr::createDummyFeatures()`](https://mlr.mlr-org.com/reference/createDummyFeatures.html)
is called.

Helper for
[`ipu()`](https://michielvandijk.github.io/ssid/reference/ipu.md).
Strips columns from seed table except for the primary id and marginal
column (as reflected in the targets tables). Also identifies factor
columns with one level and processes them before
[`mlr::createDummyFeatures()`](https://mlr.mlr-org.com/reference/createDummyFeatures.html)
is called.

## Usage

``` r
process_seed_table(df, primary_id, marginal_columns)

process_seed_table(df, primary_id, marginal_columns)
```

## Arguments

- df:

  the `data.frame` as processed by
  [`ipu()`](https://michielvandijk.github.io/ssid/reference/ipu.md)
  before this function is called.

- primary_id:

  the name of the primary ID column.

- marginal_columns:

  The vector of column names in the seed table that have matching
  targets.
