# Iterative Proportional Updating

A general case of iterative proportional fitting. It can satisfy two,
disparate sets of marginals that do not agree on a single total. A
common example is balancing population data using household- and
person-level marginal controls. This could be for survey expansion or
synthetic population creation. The second set of marginal/seed data is
optional, meaning it can also be used for more basic IPF tasks.

## Usage

``` r
ipu(
  primary_seed,
  primary_targets,
  secondary_seed = NULL,
  secondary_targets = NULL,
  primary_id = "id",
  secondary_importance = 1,
  relative_gap = 0.01,
  max_iterations = 100,
  absolute_diff = 10,
  weight_floor = 1e-05,
  verbose = FALSE,
  max_ratio = 10000,
  min_ratio = 1e-04
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

- secondary_importance:

  A `real` between 0 and 1 signifying the importance of the secondary
  targets. At an importance of 1, the function will try to match the
  secondary targets exactly. At 0, only the percentage distributions are
  used (see the vignette section "Target Agreement".)

- relative_gap:

  After each iteration, the weights are compared to the previous weights
  and the %RMSE is calculated. If the %RMSE is less than the
  `relative_gap` threshold, then the process terminates.

- max_iterations:

  maximum number of iterations to perform, even if `relative_gap` is not
  reached.

- absolute_diff:

  Upon completion, the `ipu()` function will report the worst-performing
  marginal category and geography based on the percent difference from
  the target. `absolute_diff` is a threshold below which percent
  differences don't matter.

  For example, if if a target value was 2, and the expanded weights
  equaled 1, that's a 100% difference, but is not important because the
  absolute value is only 1.

  Defaults to 10.

- weight_floor:

  Minimum weight to allow in any cell to prevent zero weights. Set to
  .0001 by default. Should be arbitrarily small compared to your seed
  table weights.

- verbose:

  Print iteration details and worst marginal stats upon completion?
  Default `FALSE`.

- max_ratio:

  `real` number. The average weight per seed record is calculated by
  dividing the total of the targets by the number of records. The
  max_scale caps the maximum weight at a multiple of that average.
  Defaults to `10000` (basically turned off).

- min_ratio:

  `real` number. The average weight per seed record is calculated by
  dividing the total of the targets by the number of records. The
  min_scale caps the minimum weight at a multiple of that average.
  Defaults to `0.0001` (basically turned off).

## Value

a `named list` with the `primary_seed` with weight, a histogram of the
weight distribution, and two comparison tables to aid in reporting.

## References

<http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.537.723&rep=rep1&type=pdf>

## Examples

``` r
hh_seed <- dplyr::tibble(
  id = c(1, 2, 3, 4),
  siz = c(1, 2, 2, 1),
  weight = c(1, 1, 1, 1),
  geo_cluster = c(1, 1, 2, 2)
)

hh_targets <- list()
hh_targets$siz <- dplyr::tibble(
  geo_cluster = c(1, 2),
  `1` = c(75, 100),
  `2` = c(25, 150)
)

result <- ipu(hh_seed, hh_targets, max_iterations = 5)
#> Error in target %>% tidyr::gather(key = category, value = count, -!!geo_colname): could not find function "%>%"
```
