
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ssid

<!-- badges: start -->
<!-- badges: end -->

The ssid package contains functions to run a dynamic spatial
microsimulation model that provides income and poverty projections at
subnational scale. It uses a spatial microsimulation approach, to derive
indicators at fine spatial resolution (e.g. districts and
municipalities) by combining socio-economic data from a household survey
(referred to as the ‘seed’), with aggregated information on population
characteristics (referred to as the ‘benchmark’) that is available for
small geographical areas from the population census and other sources.
An algorithm is used to reweigh the seed and match it with the benchmark
information to derive representative small area statistics, in this case
income and poverty indicators. In dynamic spatial microsimulation
approaches, the benchmark is projected forward and new weights are
computed for future periods to obtain trends in income distribution
under different scenarios.

## Installation

You can install the development version of ssid from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("michielvandijk/ssid")
```
