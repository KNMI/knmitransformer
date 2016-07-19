# knmitransformer

[![Travis-CI Build Status](https://travis-ci.org/MartinRoth/knmitransformer.svg?branch=master)](https://travis-ci.org/MartinRoth/knmitransformer)
[![Coverage Status](https://img.shields.io/codecov/c/github/MartinRoth/knmitransformer/master.svg)](https://codecov.io/github/MartinRoth/knmitransformer?branch=master)

'transformation program'  of the KNMI 2014 climate change scenarios for the Netherlands

**Note so far this package has no functionality and all the mentioned files below live currently in '/inst/oldFiles'**

Bakker, A. (2015), Time series transformation tool: description of the program to generate time series
consistent with the KNMI’14 climate scenarios, Technical Report TR-349, De Bilt, the Netherlands.


The actual transformation procedures for daily meteorological variables are written in R-code

- 'rr_trans_KNMI14.R' : precipitation    (sum)                [mm]             
- 'tm_trans_KNMI14.R' : temperature      (mean, min and max)  [degrees Celsius] 
- 'rsds_trans_KNMI14.R' : global radiation (sum)                [kJ/m2]

The R-functions

- 'neerslag_transformatie_KNMI14.R'
- 'temperatuur_transformatie_KNMI14.R'
- 'straling_transformatie_KNMI14.R'
   
are meant to read and transform ASCII files in a format often used by KNMI to provide data to professional users (see example *.dat files mentioned below).
   
The deltas / change factors are read from separate files 

(*official KNMI14 change factors / deltas are contained in the ASCII files 'deltas-KNMI14_\*.txt'*)

The following files provide (elemetary) examples how to use above scripts.

- 'example_neerslag_transformatie_KNMI14.R'
- 'example_temperatuur_transformatie_KNMI14.R'
- 'example_straling_transformatie_KNMI14.R'
     



##
In addition, the scripts

- 'DeBilt_v6_homogenized_neerslag_all_scenarios_KNMI14.R'
- 'DeBilt_tg_STOWA_all_scenarios_KNMI14.R'
- 'DeBilt_q_STOWA_all_scenarios_KNMI14.R'

also contain examples (for precipitation, (daily) mean temperature (tg) and global radiation) that I used myself to transform De Bilt series for all 9 scenario's at once.
The neccesary input data files for the TP scripts are also provided including (some of) of the resulting output datafiles (*.dat). 

Note the structure of the header of these input files (i.e. the 5 lines which have dates "00000000"), see:

- DeBilt_v6.dat
- DeBilt_tg_1901-2014_detrended.dat
- DeBilt_tg_1901-2014_detrended.dat

This header is very relevant but not very TP script uses it in the same way (see the *_all_scenarios_KNMI14.R scripts for an explanation).

##
This is IMPORTANT

* Transformation of different time series/stations at once is possible; stations should be separated as columns in the inputfile (no additional 
  loops are thus required in the R-script)
* GLOBAL RADIATION input should be in kJ/m2 and not in J/cm2 (the difference is a factor 10)
* GLOBAL RADIATION needs "latitude" as input; and this should be provided (for each station/column) in the fifth line in de header of the input file.
* TEMPERATURE distinguishes between minimum temperature (tn), mean temperature (tg) and maximum temperature (tx); this should be specified in the R-script
* TEMPERATURE needs the "station number" as input; and this should be provided (for each station/column) in the first line in de header of the input file 
  the "station number" is used by the TP to select the 'deltas' for the corresponding "region" in the Netherlands and uses for this the file "stationtabel" 
  (which is included in this zip-file).
  "stationtabel" currently contains 25 different stations which are listed by their official station number. 
  ALWAYS CHECK if the station that you want to transform is listed in "stationtabel", if not you have to (edit this file and) ADD it.
* For PRECIPITATION for each scenario there are three different transformations available, based on resp. the 'lower', 'centr' (=central) and 'upper' estimate 
  of the change in extreme daily precipitation. This should be specified in the R-script. The example script 'DeBilt_v6_homogenized_neerslag_all_scenarios_KNMI14.R'
  already contains a loop for this.


20150601 (c) KNMI
Jules Beersma

For questions email to:
jules.beersma@knmi.nl     
