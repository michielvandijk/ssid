# Function to fix outliers using quantile approach

This function uses quantiles to determine outliers and replace them with
the median value.

## Usage

``` r
fix_outlier_quantile(x, q_lower = 0.025, q_upper = 0.975, d = "ul")
```

## Arguments

- x:

  Vector with data containing outliers

- q_lower:

  Lower quantile range. Default is 0.025.

- q_upper:

  Upper quantile range. Default is 0.975.

- d:

  Direction where outliers will be replaced. u for upper values, l for
  lower values, ul for both.

## Examples

``` r
fix_outlier_quantile(rnorm(40, mean = 0, sd = 1), d = "ul")
#>  [1]  0.07003485 -0.63912332 -0.04996490 -0.25148344  0.44479712  0.09411486
#>  [7]  0.04653138  0.57770907  0.11819487 -1.91172049  0.86208648 -0.24323674
#> [13] -0.20608719  0.01917759  0.02956075  0.54982754  0.09411486  2.68255718
#> [19] -0.36122126  0.21335575  1.07434588 -0.66508825  1.11395242 -0.24589641
#> [25] -1.17756331 -0.97585062  1.06505732  0.13167063  0.48862881 -1.69945057
#> [31] -1.47073631  0.28415034  1.33732041  0.23669628  1.31829338  0.52390979
#> [37]  0.60674805 -0.10993567  0.17218172 -0.09032729
```
