context("Evaporation calculation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("knmitransformer_evaporation.log"))
library(data.table)

inputTemp   <- "regressionInput/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"
inputRad <- "regressionInput/KNMI14____ref_rsds___19810101-20101231_wrong2.txt"
ofile      <- NA
regions    <- "MON" # table that links stations to region
rounding   <- FALSE

test_that("differences in header are discovered", {
  expect_error(TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                scenario = "GL",
                horizon = 2030,
                regions = regions,
                rounding = rounding),
               "Same stations should be used for temperature and radiation")
})

context("evmk transformation - Entire station set")

inputTemp   <- system.file("refdata",
                          "KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                          package = "knmitransformer")
inputRad <- system.file("refdata",
                          "KNMI14____ref_rsds___19810101-20101231_v3.2.txt",
                          package = "knmitransformer")
ofile    <- NA
regions  <- MatchRegionsOnStationId(ReadInput("tg", inputTemp)$header[1, -1])
rounding <- FALSE

test_that("2030 decadal prediction", {
  scenario <- "GL"
  horizon  <- 2030


  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = "tmp.txt",
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding
                       )

  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk.rds")

  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = TRUE)

  expect_equal_to_reference(tmp,
    "regressionOutput/evaporation/KNMI14___2030_evmk_rounded.rds")
})

test_that("Scenario WL", {
  scenario <- "WL"
  horizon  <- 2050
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2050_evmk.rds")

  # Difference of 0.07 instead of the usual 0.01
  horizon <-  2085
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2085_evmk.rds")
})

test_that("Scenario WH", {
  scenario <- "WH"
  horizon  <- 2050
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2050_evmk.rds")

  horizon <-  2085
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2085_evmk.rds")
})

test_that("Scenario GH", {
  scenario <- "GH"
  horizon  <- 2050
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2050_evmk.rds")

  horizon <-  2085
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2085_evmk.rds")
})

test_that("Scenario GL", {
  scenario <- "GL"
  horizon  <- 2050
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2050_evmk.rds")

  horizon <- 2085
  tmp <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                       ofile = ofile,
                       scenario = scenario,
                       horizon = horizon,
                       regions = regions,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2085_evmk.rds")
})
