# Applies an importance weight to an ipfr factor

At lower values of importance, the factor is moved closer to 1.

## Usage

``` r
adjust_factor(factor, importance)
```

## Arguments

- factor:

  A correction factor that is calculated using target/current.

- importance:

  A `real` between 0 and 1 signifying the importance of the factor. An
  importance of 1 does not modify the factor. An importance of 0.5 would
  shrink the factor closer to 1.0 by 50 percent.

## Value

The adjusted factor.
