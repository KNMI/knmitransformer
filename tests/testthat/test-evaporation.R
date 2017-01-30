context("Evaporation calculation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_evaporation.log'))
library(data.table)

context("evmk transformation - Entire station set")

ifile_tg   <- "regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2.txt"
ifile_rsds <- "regressionInput/radiation/KNMI14____ref_rsds___19810101-20101231_v3.2.txt"
ofile      <- NA
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  sc="GL"

  horizon = 2030


  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile="tmp.txt",
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)

  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk.rds")
})

test_that("Scenario WL", {
  sc="WL"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2050_evmk.rds")

  # Difference of 0.07 instead of the usual 0.01
  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2085_evmk.rds")
})

test_that("Scenario WH", {
  sc="WH"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2050_evmk.rds")

  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2085_evmk.rds")
})

test_that("Scenario GH", {
  sc="GH"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2050_evmk.rds")

  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2085_evmk.rds")
})

test_that("Scenario GL", {
  sc="GL"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2050_evmk.rds")

  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   horizon = horizon,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2085_evmk.rds")
})