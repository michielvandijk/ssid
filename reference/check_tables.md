# Check seed and target tables for completeness

Given seed and targets, checks to make sure that at least one
observation of each marginal category exists in the seed table.
Otherwise, ipf/ipu would produce wrong answers without throwing errors.

## Usage

``` r
check_tables(
  primary_seed,
  primary_targets,
  secondary_seed = NULL,
  secondary_targets = NULL,
  primary_id
)
```

## Value

both seed tables and target lists
