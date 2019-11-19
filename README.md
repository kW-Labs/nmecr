# nmecr

## Overview

nmecr is a Measurement & Verfication practitioner's toolbox that builds upon the energy efficiency community's past efforts to model complex and nuanced building energy use profiles. While intended for use in the commercial and institutional sectors, its functions can be extended to model the energy use profile of industrial systems.

Energy Modeling Algorithms available:

  - Simple Linear Regression with Outside Air Temperature
  - Three Parameter Linear Model (Cooling/Heating) 
  - Four Parameter Linear Model
  - Five Parameter Linear Model
  - Time-of-Week Model
  - Temperature & Time-of-Week Model
  - Heating Degree Day Model
  - Cooling Degree Day Model
  - Heating & Cooling Degree Day Model

## Installation

You can nmecr from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("kW-Labs/nmecr")

# If a dependecy package is missing, use:
devtools::install_deps(".")

```

## Getting help

If you have a question or encounter a bug, please file a minimal reproducible example
on [github](https://github.com/kW-Labs/nmecr/issues). 
-----
