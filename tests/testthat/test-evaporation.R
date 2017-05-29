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
rounding   <- FALSE

test_that("2030 decadal prediction", {
  scenario="GL"

  horizon = 2030


  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile="tmp.txt",
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding
                       )

  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk.rds")

  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = TRUE)

  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk_rounded.rds")
})

test_that("Scenario WL", {
  scenario="WL"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2050_evmk.rds")

  # Difference of 0.07 instead of the usual 0.01
  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2085_evmk.rds")
})

test_that("Scenario WH", {
  scenario="WH"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2050_evmk.rds")

  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2085_evmk.rds")
})

test_that("Scenario GH", {
  scenario="GH"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2050_evmk.rds")

  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2085_evmk.rds")
})

test_that("Scenario GL", {
  scenario="GL"

  horizon = 2050
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2050_evmk.rds")

  horizon =  2085
  tmp <- TransformEvap(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       regio.file = regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2085_evmk.rds")
})