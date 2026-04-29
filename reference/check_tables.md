# Check seed and target tables for completeness

Given seed and targets, checks to make sure that at least one
observation of each marginal category exists in the seed table.
Otherwise, ipf/ipu would produce wrong answers without throwing errors.

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

check_tables(
  primary_seed,
  primary_targets,
  secondary_seed = NULL,
  secondary_targets = NULL,
  primary_id
)
```

## Arguments

- primary_seed:

  In population synthesis or household survey expansion, this would be
  the household seed table (each record would represent a household). It
  could also be a trip table, where each row represents an
  origin-destination pair.

- primary_targets:

  A `named list` of data frames. Each name in the list defines a
  marginal dimension and must match a column from the `primary_seed`
  table. The data frame associated with each named list element can
  contain a geography field (starting with "geo\_"). If so, each row in
  the target table defines a new geography (these could be TAZs, tracts,
  clusters, etc.). The other column names define the marginal categories
  that targets are provided for. The vignette provides more detail.

- secondary_seed:

  Most commonly, if the primary_seed describes households, the secondary
  seed table would describe the persons in each household. Must contain
  the same `primary_id` column that links each person to their
  respective household in `primary_seed`.

- secondary_targets:

  Same format as `primary_targets`, but they constrain the
  `secondary_seed` table.

- primary_id:

  The field used to join the primary and secondary seed tables. Only
  necessary if `secondary_seed` is provided.

## Value

both seed tables and target lists

both seed tables and target lists
