context("Radiation transformation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("knmitransformer_radiation.log"))
library(data.table)

context("rsds transformation - Entire station set")

input    <- system.file("refdata",
                        "KNMI14____ref_rsds___19810101-20101231_v3.2.txt",
                        package="knmitransformer")
ofile    <- NA
rounding <- FALSE

test_that("2030 decadal prediction", {
  scenario="GL"

  horizon = 2030
  tmp <- TransformRadiation(input=input,
                            ofile="tmp.txt",
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14___2030_rsds.rds")

  tmp <- TransformRadiation(input=input,
                            ofile="tmp.txt",
                            scenario=scenario,
                            horizon = horizon,
                            rounding = TRUE)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14___2030_rsds_rounded.rds")
})

test_that("Scenario GL", {
  scenario="GL"

  horizon = 2050
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_GL_2050_rsds.rds")

  horizon = 2085
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_GL_2085_rsds.rds")
})

test_that("Scenario GH", {
  scenario="GH"

  horizon = 2050
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_GH_2050_rsds.rds")

  horizon = 2085
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_GH_2085_rsds.rds")
})

test_that("Scenario WH 2050", {
  scenario="WH"

  horizon = 2050
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_WH_2050_rsds.rds")
})

test_that("Scenario WH 2085", {
  #skip_on_travis()
  scenario="WH"
  horizon = 2085
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)

  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_WH_2085_rsds.rds")
})

test_that("Scenario WL", {
  scenario="WL"

  horizon = 2050
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_WL_2050_rsds.rds")

  horizon = 2085
  tmp <- TransformRadiation(input=input,
                            ofile=ofile,
                            scenario=scenario,
                            horizon = horizon,
                            rounding = rounding)
  expect_equal_to_reference(tmp,
      "regressionOutput/radiation/KNMI14_WL_2085_rsds.rds")
})

test_that("Check input latitude", {
  input1     <- ReadInput("rsds", system.file("refdata",
                          "KNMI14____ref_rsds___19810101-20101231_v3.2.txt",
                          package="knmitransformer"))

  input2 <- input1
  input2$header <- input2$header[, c(1,10)]
  input2$obs    <- input2$obs[, c(1,10)]

  expect_error(TransformRadiation(input2, rounding = rounding),
               "Number of stations does not match length of latitude vector.")
})
