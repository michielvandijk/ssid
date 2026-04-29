# Scale targets to ensure consistency

Often, different marginals may disagree on the total number of units. In
the context of household survey expansion, for example, one marginal
might say there are 100k households while another says there are 101k.
This function solves the problem by scaling all target tables to match
the first target table provided.

Often, different marginals may disagree on the total number of units. In
the context of household survey expansion, for example, one marginal
might say there are 100k households while another says there are 101k.
This function solves the problem by scaling all target tables to match
the first target table provided.

## Usage

``` r
scale_targets(targets, verbose = FALSE)

scale_targets(targets, verbose = FALSE)
```

## Arguments

- targets:

  `named list` of `data.frames` in the same format required by
  [ipu](https://michielvandijk.github.io/ssid/reference/ipu.md).

- verbose:

  `logical` Show a warning for each target scaled? Defaults to `FALSE`.

## Value

A `named list` with the scaled targets

A `named list` with the scaled targets
