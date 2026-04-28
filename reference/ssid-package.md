# ssid: Spatial Simulation of Income Dynamics model

The package contains functions to run a dynamic spatial microsimulation
model that provides income and poverty projections at subnational scale.
It uses a spatial microsimulation approach, to derive indicators at fine
spatial resolution (e.g. districts and municipalities) by combining
socio-economic data from a household survey (referred to as the 'seed'),
with aggregated information on population characteristics (referred to
as the 'benchmark') that is available for small geographical areas from
the population census and other sources. An algorithm is used to reweigh
the seed and match it with the benchmark information to derive
representative small area statistics, in this case income and poverty
indicators. In dynamic spatial microsimulation approaches, the benchmark
is projected forward and new weights are computed for future periods to
obtain trends in income distribution under different scenarios.

## See also

Useful links:

- <https://github.com/michielvandijk/ssid>

- <https://michielvandijk.github.io/ssid/>

- Report bugs at <https://github.com/michielvandijk/ssid/issues>

## Author

**Maintainer**: Michiel Van Dijk <michiel.vandijk@wur.nl>
([ORCID](https://orcid.org/0000-0002-5207-7304))
