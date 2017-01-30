context("Radiation transformation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_radiation.log'))
library(data.table)

context("rsds transformation - Entire station set")

ifile="regressionInput/radiation/KNMI14____ref_rsds___19810101-20101231_v3.2.txt"
ofile      <- NA
delta.file <- NA

test_that("2030 decadal prediction", {
  sc="GL"

  horizon = 2030
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile="tmp.txt",
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14___2030_rsds.rds")
})

test_that("Scenario GL", {
  sc="GL"

  horizon = 2050
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GL_2050_rsds.rds")

  horizon = 2085
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GL_2085_rsds.rds")
})

test_that("Scenario GH", {
  sc="GH"

  horizon = 2050
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GH_2050_rsds.rds")

  horizon = 2085
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GH_2085_rsds.rds")
})

test_that("Scenario WH 2050", {
  sc="WH"

  horizon = 2050
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WH_2050_rsds.rds")
})

test_that("Scenario WH 2085", {
  #skip_on_travis()
  sc="WH"
  horizon = 2085 # This set is not produced by this version of the code - neither with
  # a factor of 0.7 or 0.75 with old delta file it works
  oldDeltaFile = "regressionInput/radiation/deltas-KNMI14__rsds_WH__2085_v201504.txt"
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=oldDeltaFile,
                                          sc=sc,
                                          horizon = horizon)

  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WH_2085_rsds.rds")
})

test_that("Scenario WL", {
  sc="WL"

  horizon = 2050
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WL_2050_rsds.rds")

  horizon = 2085
  tmp <- TransformRadiation(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          horizon = horizon)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WL_2085_rsds.rds")
})
