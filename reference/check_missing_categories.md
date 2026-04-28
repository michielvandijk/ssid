# Check for missing categories in seed

Helper function for `check_tables`.

## Usage

``` r
check_missing_categories(seed, target, target_name, geo_colname)
```

## Arguments

- seed:

  seed table to check

- target:

  data.frame of a single target table

- target_name:

  the name of the target (e.g. size)

- geo_colname:

  the name of the geo column in both the `seed` and `target` (e.g.
  geo_taz)

## Value

Nothing. Throws an error if one is found.
