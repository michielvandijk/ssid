# Check geo fields

Helper function for
[`check_tables`](https://michielvandijk.github.io/ssid/reference/check_tables.md).
Makes sure that geographies in a seed and target table line up properly.

## Usage

``` r
check_geo_fields(seed, target, target_name)
```

## Arguments

- seed:

  seed table to check

- target:

  data.frame of a single target table

- target_name:

  the name of the target (e.g. size)

## Value

The seed and target table (which may be modified)
