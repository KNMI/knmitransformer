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

  p=2030
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile="tmp.txt",
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14___2030_rsds.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GL_2050_rsds.rds")

  p=2085
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GL_2085_rsds.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GH_2050_rsds.rds")

  p=2085
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_GH_2085_rsds.rds")
})

test_that("Scenario WH 2050", {
  sc="WH"

  p=2050
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WH_2050_rsds.rds")
})

test_that("Scenario WH 2085", {
  #skip_on_travis()
  sc="WH"
  p=2085 # This set is not produced by this version of the code - neither with
  # a factor of 0.7 or 0.75
  oldDeltaFile = "regressionInput/radiation/deltas-KNMI14__rsds_WH__2085_v201504.txt"
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=oldDeltaFile,
                                          sc=sc,
                                          p=p)

  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WH_2085_rsds.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WL_2050_rsds.rds")

  p=2085
  tmp <- straling_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p)
  expect_equal_to_reference(tmp, "regressionOutput/radiation/KNMI14_WL_2085_rsds.rds")
})