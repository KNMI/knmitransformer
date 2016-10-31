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

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                   ofile=ofile,
                                   delta.file=delta.file,
                                   sc=sc,
                                   p=p,
                                   var=var
                                   )

  expect_equal_to_reference(tmp, "regressionTestOutput/uitvoertg_without_regional.rds")

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                   ofile=ofile,
                                   delta.file=delta.file,
                                   sc="GL",
                                   p=2050,
                                   var=var
                                   )

  expect_equal_to_reference(tmp, "regressionTestOutput/uitvoertg_GL_2050.rds")

  expect_error(temperatuur_transformatie_KNMI14(ifile=ifile,
                                   ofile=ofile,
                                   delta.file=delta.file,
                                   sc="GL",
                                   p=NA,
                                   var=var
                                   ),
               "Period must be valid, i.e. 2030, 2050, or 2085")

})

test_that("Temperature regression test (with actual data)", {
  #filename <- system.file("ReferenceData", "KNMI14____ref__tg___19810101-20101231_v1.0_T25.txt", package="knmitransformer")
  #tmp <- fread(filename, skip = 1)
  #colnames <- c("date", paste0("Station_", as.integer(tmp[1, -1, with = FALSE])))
  #observations <- tmp[-(1:5), ]
  #setnames(observations, colnames)
  #expect_equal(3*2,9)
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"     # input file
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
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
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg___DeBilt.rds")

  #ref <- fread("regressionInput/temperature/KNMI14___2030_tg___19810101-20101231_v3.2_260.txt")
  #expect_equal(tmp[, .(V1, round(V2, 1))], ref[, .(V1, round(V2, 1))])
})
