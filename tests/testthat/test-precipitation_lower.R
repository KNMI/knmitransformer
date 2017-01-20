context("Transformation of precipitation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_precipitation.log'))
library(data.table)

context("rr transformation (lower) - Entire station set")

scaling    <- "lower"
ifile      <- "regressionInput/precipitation/KNMI14____ref_rrlower___19810101-20101231_v3.2.txt"
ofile      <- NA
delta.file <- NA

test_that("2030 decadal prediction", {
  sc="GL"

  p=2030
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14___2030_rr_lower.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GL_2050_rr_lower.rds")

  p=2085
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GL_2085_rr_lower.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GH_2050_rr_lower.rds")

  p=2085
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_GH_2085_rr_lower.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WH_2050_rr_lower.rds")

  p=2085
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WH_2085_rr_lower.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WL_2050_rr_lower.rds")

  p=2085
  tmp <- neerslag_transformatie_KNMI14(ifile=ifile,
                                       ofile=ofile,
                                       delta.file=delta.file,
                                       sc=sc,
                                       p=p,
                                       scaling=scaling)
  expect_equal_to_reference(tmp, "regressionOutput/precipitation/KNMI14_WL_2085_rr_lower.rds")
})

