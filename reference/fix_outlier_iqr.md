# Function to fix outliers using interquartile range

This function uses the interquartile range to determine outliers and
replace them with the median or a value based on the interquartile
range.

## Usage

``` r
fix_outlier_iqr(x, r = "m", f = 1.5, d = "ul")
```

## Arguments

- x:

  Vector with data containing outliers

- r:

  Values to replace the outliers. Either m for median (default) or q for
  0.25 quantile range - f \* IQR (lower) and/or 0.75 quantile range + f
  \* IQR (upper).

- f:

  factor to determine outliers. Default is 1.5. Outliers are measured as
  values that are f \* IQR above or below the .25 and .75 quantile
  range, respectively.

- d:

  Direction where outliers will be replaced. u for upper values, l for
  lower values, ul for both.

## Examples

``` r
fix_outlier_iqr(rnorm(40, mean = 0, sd = 1))
#>  [1] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721
#>  [6]  1.148411606 -1.821817661 -0.247325302 -0.244199607 -0.282705449
#> [11] -0.553699384  0.628982042  2.065024895 -1.630989402  0.512426950
#> [16] -1.863011492 -0.522012515 -0.052601910  0.542996343 -0.914074827
#> [21]  0.468154420  0.362951256 -1.304543545  0.737776321  1.888504929
#> [26] -0.097445104 -0.935847354 -0.015950311 -0.826788954 -1.512399651
#> [31]  0.935363190  0.176488611  0.243685465  1.623548883  0.112038083
#> [36] -0.133997013 -1.910087468 -0.279237242 -0.313445978  1.067307879
```
