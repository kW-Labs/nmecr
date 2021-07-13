# nmecr

## Overview

nmecr is a Measurement & Verification practitioner's toolbox that builds upon the energy efficiency community's past efforts to model complex and nuanced building energy use profiles. While intended for use in the commercial and institutional sectors, its functions can be extended to model the energy use profile of industrial systems.

There are ten energy modeling algorithms available in nmecr:

1. For use with hourly, daily, and monthly time interval data:
  - Simple Linear Regression with Outside Air Temperature
  - Three Parameter Linear Model (Cooling/Heating) 
  - Four Parameter Linear Model
  - Five Parameter Linear Model
  
2. For use with hourly and daily time interval data:
  - Time-of-Week Model
  - Temperature & Time-of-Week Model
  
3. For use with monthly time interval data only:
  - Heating Degree Day Model
  - Cooling Degree Day Model
  - Heating & Cooling Degree Day Model

## Installation

You can download nmecr from GitHub.

``` r
install.packages("devtools")
devtools::install_github("kW-Labs/nmecr", upgrade = "never")
```
Please make sure R is updated on your system. Check [here](https://www.r-project.org/) for the latest release.

## Getting help



If you have a question or encounter a bug, please file a minimal reproducible example on [github](https://github.com/kW-Labs/nmecr/issues).




