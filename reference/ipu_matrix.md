# Balance a matrix given row and column targets

This function simplifies the call to `ipu()` for the simple case of a
matrix and row/column targets.

## Usage

``` r
ipu_matrix(mtx, row_targets, column_targets, ...)
```

## Arguments

- mtx:

  a `matrix`

- row_targets:

  a vector of targets that the row sums must match

- column_targets:

  a vector of targets that the column sums must match

- ...:

  additional arguments that are passed to `ipu()`. See `ipu` for
  details.

## Value

A `matrix` that matches row and column targets

## Examples

``` r
mtx <- matrix(data = runif(9), nrow = 3, ncol = 3)
row_targets <- c(3, 4, 5)
column_targets <- c(5, 4, 3)
ipu_matrix(mtx, row_targets, column_targets)
#> Error in seed %>% dplyr::mutate(geo_all = 1, id = seq(1, n())): could not find function "%>%"
```
