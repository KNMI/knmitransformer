context("Evaporation calculation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_evaporation.log'))
library(data.table)

context("evmk transformation - Entire station set")

var="evmk"
ifile_tg="tests/testthat/regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2.txt"
ifile_rsds="tests/testthat/regressionInput/radiation/KNMI14____ref_rsds___19810101-20101231_v3.2.txt"
ofile      <- "tmp.txt" # output file - used only temporary
delta.file.rsds <- NA
delta.file.tg <- NA
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  sc="GL"

  p=2030


  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)

  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14___2030_evmk.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_WL_2050_evmk.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_WL_2085_evmk.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_WH_2050_evmk.rds")

  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_WH_2085_evmk.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_GH_2050_evmk.rds")

  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_GH_2085_evmk.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_GL_2050_evmk.rds")

  p = 2085
  tmp <- droogte_berekening_KNMI14(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                                   ofile=ofile,
                                   delta.file.rsds=delta.file.rsds,
                                   delta.file.tg = delta.file.tg,
                                   sc=sc,
                                   p=p,
                                   regio.file = regio.file)
  expect_equal_to_reference(tmp, "tests/testthat/regressionOutput/evaporation/KNMI14_GL_2085_evmk.rds")
})