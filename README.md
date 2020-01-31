# nmecr

## Overview

nmecr is a Measurement & Verfication practitioner's toolbox that builds upon the energy efficiency community's past efforts to model complex and nuanced building energy use profiles. While intended for use in the commercial and institutional sectors, its functions can be extended to model the energy use profile of industrial systems.

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
devtools::install_github("kW-Labs/nmecr")
```
Please make sure R is updated on your system. Check [here](https://www.r-project.org/) for the latest release.

If the download fails, try and read the error message in the console to fix the issues. Here is a list of common errors and their fixes:

1. *Rtools*: RTools is required to build R packages, but no version of Rtools...found. 
	
	Fix: Download the latest recommended version of [RTools](https://cran.r-project.org/bin/windows/Rtools/). Make sure check the option for setting the PATH environment variable at the time of installation.
	
	To check if RTools is correctly installed:	
``` r
install.packages("pkgbuild")
library(pkgbuild)
find_rtools() # should return TRUE
```
	
2. *rlang*: Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  namespace ‘rlang’ 0.2.2 is being loaded, but >= 0.4.4 is required...failed to lock directory ‘system_path/R/3.6.2’ for modifying. Try removing ‘system_path/3.6.2/00LOCK-rlang.
  
  You might get this error while downloading Tidyverse
	
  Fix: Navigate to the directory where 00LOCK-rlang file is located and manually delete it. Close all R sessions.
  	Install and load rlang in a fresh session:
	
``` r
install.packages("rlang")
require(rlang)
```



## Getting help

If you have a question or encounter a bug, please file a minimal reproducible example on [github](https://github.com/kW-Labs/nmecr/issues). 

