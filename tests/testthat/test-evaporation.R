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

  p=2030


  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)

  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2050_evmk.rds")

  # Difference of 0.07 instead of the usual 0.01
  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2085_evmk.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2050_evmk.rds")

  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2085_evmk.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2050_evmk.rds")

  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2085_evmk.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2050_evmk.rds")

  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2085_evmk.rds")
})