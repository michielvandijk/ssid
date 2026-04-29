# Balances secondary targets to primary

The average weight per record needed to satisfy targets is computed for
both primary and secondary targets. Often, these can be very different,
which leads to poor performance. The algorithm must use extremely large
or small weights to match the competing goals. The secondary targets are
scaled so that they are consistent with the primary targets on this
measurement.

The average weight per record needed to satisfy targets is computed for
both primary and secondary targets. Often, these can be very different,
which leads to poor performance. The algorithm must use extremely large
or small weights to match the competing goals. The secondary targets are
scaled so that they are consistent with the primary targets on this
measurement.

## Usage

``` r
balance_secondary_targets(
  primary_targets,
  primary_seed,
  secondary_targets,
  secondary_seed,
  secondary_importance,
  primary_id
)

balance_secondary_targets(
  primary_targets,
  primary_seed,
  secondary_targets,
  secondary_seed,
  secondary_importance,
  primary_id
)
```

## Arguments

- primary_targets:

  A `named list` of data frames. Each name in the list defines a
  marginal dimension and must match a column from the `primary_seed`
  table. The data frame associated with each named list element can
  contain a geography field (starting with "geo\_"). If so, each row in
  the target table defines a new geography (these could be TAZs, tracts,
  clusters, etc.). The other column names define the marginal categories
  that targets are provided for. The vignette provides more detail.

- primary_seed:

  In population synthesis or household survey expansion, this would be
  the household seed table (each record would represent a household). It
  could also be a trip table, where each row represents an
  origin-destination pair.

- secondary_targets:

  Same format as `primary_targets`, but they constrain the
  `secondary_seed` table.

- secondary_seed:

  Most commonly, if the primary_seed describes households, the secondary
  seed table would describe the persons in each household. Must contain
  the same `primary_id` column that links each person to their
  respective household in `primary_seed`.

- secondary_importance:

  A `real` between 0 and 1 signifying the importance of the secondary
  targets. At an importance of 1, the function will try to match the
  secondary targets exactly. At 0, only the percentage distributions are
  used (see the vignette section "Target Agreement".)

- primary_id:

  The field used to join the primary and secondary seed tables. Only
  necessary if `secondary_seed` is provided.

## Value

`named list` of the secondary targets

`named list` of the secondary targets

## Details

If multiple geographies are present in the secondary_target table, then
balancing is done for each geography separately.

If multiple geographies are present in the secondary_target table, then
balancing is done for each geography separately.
