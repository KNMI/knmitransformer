context("Temperature transformation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer.log'))
library(data.table)

test_that("Temperature regression test", {
  ifile="DeBilt_tg_1901-2014_detrended.dat"     # input file
  #ifile="./tests/testthat/DeBilt_tg_1901-2014_detrended.dat"     # input file
  ofile="uitvoertg.txt"      # output file (DEFAULT="uitvoer.txt")
  delta.file=NA            # file containing deltas
  # if delta.file is not provided (DEFAULT)
  # KNMI'14 deltas are used

  # deltas are derived from KNMI'14 deltas if delta.file is not specified
  # following arguments are used
  sc="GL"                    # scenario ("GL", "GH", "WL", "WH")
  p=2030                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                   ofile=ofile,
                                   delta.file=delta.file,
                                   sc=sc,
                                   p=p,
                                   var=var,
                                   regio.file=regio.file)

  expect_equal_to_reference(tmp, "regressionTestOutput/uitvoertg.rds")
})

