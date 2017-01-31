context("Transformation of precipitation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_precipitation.log'))
library(data.table)

context("rr transformation (upper) - Entire station set")

subscenario    <- "upper"
ifile      <- "regressionInput/precipitation/KNMI14____ref_rrupper___19810101-20101231_v3.2.txt"
ofile      <- NA
delta.file <- NA

test_that("2030 decadal prediction", {
  scenario="GL"

  horizon = 2030
  tmp <- TransformPrecip(ifile=ifile,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14___2030_rr_upper.rds")
})

test_that("Scenario GL", {
  scenario="GL"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GL_2050_rr_upper.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GL_2085_rr_upper.rds")
})


test_that("Scenario GH", {
  scenario="GH"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GH_2050_rr_upper.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GH_2085_rr_upper.rds")
})

test_that("Scenario WH", {
  scenario="WH"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WH_2050_rr_upper.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WH_2085_rr_upper.rds")
})

test_that("Scenario WL", {
  scenario="WL"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WL_2050_rr_upper.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       scenario=scenario,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WL_2085_rr_upper.rds")
})
