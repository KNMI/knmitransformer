### Version 0.2.4

- made GL with horizon 2030 the default scenario (for horizon 2030 the choice
  of the scenario does not influence anything)
  
### Version 0.2.3

- input replaced infile and can now be an object of class knmiTF
- CreateKnmiTFInput aids the user in the creation of own input (single station
  only so far)
- Added examples for user defined time series in the vignette

Bug fixes:
- regio.file did not influence the outcome
- TransformPrecip did not work for sinlge stations
  
### Version 0.2.1

- moved reference files fully to the revData folder

### Version 0.1.7

- made rounding optional (on by default) this allows more thoroughly testing

### Version 0.1.6

- added AppVeyor support (Windows build)
- improved file description in WriteOutput
- added ShowStationTable
- switch to a factor of 0.75 in rsds

### Version 0.1.5

- renamed the functions (English / Google code style)
- renamed 'sc' to scenario
- renamed 'p' argument to 'horizon'
- renamed 'scaling' to subscenario
- removed 'delta.file' argument (introduced new reference set for RSDS WH 2085)
- removed 'dryingScheme' argument


### Version 0.1.4

- introduced C++ uniroot 

### Version 0.1.3

- Splitted rr_trans, rsds_trans into more comprehensible smaller functions
- Separated drying scheme from version number
- Simplified makkink (needs further attention)
- Introduced profiling into the workflow

### Version 0.1.2

- Transition to package functionality
- Added automatic regression tests
- Condensed code
