
<!-- README.md is generated from README.Rmd. Please edit that file -->
knmitransformer
===============

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Travis-CI Build Status](https://travis-ci.org/MartinRoth/knmitransformer.svg?branch=master)](https://travis-ci.org/MartinRoth/knmitransformer) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/MartinRoth/knmitransformer?branch=master&svg=true)](https://ci.appveyor.com/project/MartinRoth/knmitransformer) [![Coverage Status](https://img.shields.io/codecov/c/github/MartinRoth/knmitransformer/master.svg)](https://codecov.io/github/MartinRoth/knmitransformer?branch=master)

Please, if there is an issue of any kind, file it [here](https://github.com/MartinRoth/knmitransformer/issues)

The 'transformation program' of the KNMI 2014 climate change scenarios for the Netherlands. With this program current climate time series are transformed to represent the climate of the KNMI 2014 scenarios for 2030, 2050, and 2085.

To install the package use:

``` r
library(devtools)
install_github("MartinRoth/knmitransformer")
```

or

``` r
devtools::install_github("MartinRoth/knmitransformer")
```

### Content

The following R-functions for the transformation of daily meteorological variables are provided:

-   'TransformPrecip' precipitation (sum) \[mm\]
-   'TransformTemp' temperature (mean, min and max) \[degrees Celsius\]
-   'TransformRadiation' global radiation (sum) \[kJ/m2\]
-   'TransformEvap' Makkink Evaporation (sum) \[mm\]

The routines and the used change factors are developed for the use within the Netherlands.

One can inspect the official KNMI14 change factors / deltas in the 'inst/extData' folder.

### Note

-   Transformation of different time series/stations at once is possible; stations should be separated as columns in the inputfile
-   GLOBAL RADIATION needs "latitude" as input; and this should be provided (for each station/column) in the fifth line in de header of the input file.
-   TEMPERATURE distinguishes between minimum temperature (tn), mean temperature (tg) and maximum temperature (tx); this should be specified in the function call
-   TEMPERATURE and EVAPORATION need the "station number" as input. The station table makes the link between station number and region within the Netherlands. For the reference data sets the stationtable is already included, see
    `ShowStationTable()`. *Always check* if the station that you want to transform is listed there, if not you have to create a new station table file and include the file name as argument.
-   For PRECIPITATION there are three different subscenarios for each scenario available, based on resp. the 'lower', 'centr' (=central) and 'upper' estimate of the change in extreme daily precipitation. This should be specified in the function call, the default is set to 'centr'.

### Reference

Bakker, A. (2015), Time series transformation tool: description of the program to generate time series consistent with the KNMI’14 climate scenarios, Technical Report TR-349, De Bilt, the Netherlands.
