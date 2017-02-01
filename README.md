# knmitransformer

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/MartinRoth/knmitransformer.svg?branch=master)](https://travis-ci.org/MartinRoth/knmitransformer)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/MartinRoth/knmitransformer?branch=master&svg=true)](https://ci.appveyor.com/project/MartinRoth/knmitransformer)
[![Coverage Status](https://img.shields.io/codecov/c/github/MartinRoth/knmitransformer/master.svg)](https://codecov.io/github/MartinRoth/knmitransformer?branch=master)

Please, if there is an issue of any kind, file it 
[here](https://github.com/MartinRoth/knmitransformer/issues) 

The 'transformation program' of the KNMI 2014 climate change scenarios for the
Netherlands. With this program current climate time series are transformed to
represent the climate of the KNMI 2014 scenarios for 2030, 2050, and 2085.

The following R-functions for the transformation of daily meteorological 
variables are provided:

- 'TransformPrecip'      precipitation       (sum)                [mm] 
- 'TransformTemp'        temperature         (mean, min and max)  [degrees Celsius] 
- 'TransformRadiation'   global radiation    (sum)                [kJ/m2]
- 'TransformEvap'        Makkink Evaporation (sum)                [mm]

The routines and the used change factors are developed for the use within the
Netherlands.

One can inspect the official KNMI14 change factors / deltas in the 
'inst/extData' folder.


##
This is IMPORTANT

* Transformation of different time series/stations at once is possible; stations
  should be separated as columns in the inputfile 
* GLOBAL RADIATION needs "latitude" as input; and this should be provided (for
  each station/column) in the fifth line in de header of the input file.
* TEMPERATURE distinguishes between minimum temperature (tn), mean temperature
  (tg) and maximum temperature (tx); this should be specified in the function
  call
* TEMPERATURE needs the "station number" as input; and this should be provided
  (for each station/column) in the first line in de header of the input file the
  "station number" is used by the TP to select the 'deltas' for the
  corresponding "region" in the Netherlands and uses for this the file
  "stationtabel" (which is included in 'inst/extdata').
  "stationtabel" currently contains 25 different stations which are listed by
  their official station number. 
  ALWAYS CHECK if the station that you want to transform is listed in
  "stationtabel", if not you have to (edit this file and) ADD it.
* For PRECIPITATION for each scenario there are three different subscenarios
  available, based on resp. the 'lower', 'centr' (=central) and 'upper' estimate 
  of the change in extreme daily precipitation. This should be specified in the
  function call, the default is set to 'centr'.
  
##
For more information, see:  
Bakker, A. (2015), Time series transformation tool: description of the program
to generate time series consistent with the KNMI’14 climate scenarios, Technical
Report TR-349, De Bilt, the Netherlands.
