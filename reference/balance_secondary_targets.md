# Balances secondary targets to primary

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
```

## Value

`named list` of the secondary targets

## Details

If multiple geographies are present in the secondary_target table, then
balancing is done for each geography separately.
