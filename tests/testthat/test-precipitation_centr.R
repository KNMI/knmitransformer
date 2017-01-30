context("Transformation of precipitation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_precipitation.log'))
library(data.table)


context("rr transformation (centr) - Entire station set")

subscenario    <- "centr"
ifile      <- "regressionInput/precipitation/KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt"
ofile      <- NA

test_that("2030 decadal prediction", {
  sc="GL"

  horizon = 2030
  tmp <- TransformPrecip(ifile=ifile,
                                          ofile="tmp.txt",
                                          sc=sc,
                                          horizon = horizon,
                                          subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14___2030_rr_centr.rds")
})

test_that("Scenario GL", {
  sc="GL"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GL_2050_rr_centr.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GL_2085_rr_centr.rds")
})

test_that("Scenario GH", {
  sc="GH"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GH_2050_rr_centr.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GH_2085_rr_centr.rds")
})

test_that("Scenario WH", {
  sc="WH"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WH_2050_rr_centr.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WH_2085_rr_centr.rds")
})

test_that("Scenario WL", {
  sc="WL"

  horizon = 2050
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WL_2050_rr_centr.rds")

  horizon = 2085
  tmp <- TransformPrecip(ifile=ifile,
                                       ofile=ofile,
                                       sc=sc,
                                       horizon = horizon,
                                       subscenario=subscenario)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WL_2085_rr_centr.rds")
})

