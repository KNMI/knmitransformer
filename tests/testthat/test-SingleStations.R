context("SingleStations")

rounding <- FALSE

## -----------------------------------------------------------------------------
## precipitation
## -----------------------------------------------------------------------------

input1 <- ReadInput("rr",
    KnmiRefFile("KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt"))

input2 <- input1
input2$header <- input2$header[, c(1, 10)]
input2$obs    <- input2$obs[, c(1, 10)]

test_that("precipitation", {
  input1Trans <- TransformPrecip(input1, rounding = rounding)
  input2Trans <- TransformPrecip(input2, rounding = rounding)

  expect_true(all(input1Trans[, c(1, 10)] == input2Trans))
})


## -----------------------------------------------------------------------------
## temperature
## -----------------------------------------------------------------------------
var        <- "tg"
regio.file <- "stationstabel" # table that links stations to region
input1     <- ReadInput(var,
    KnmiRefFile("KNMI14____ref_tg___19810101-20101231_v3.2.txt"))

input2 <- input1
input2$header <- input2$header[, c(1, 10)]
input2$obs    <- input2$obs[, c(1, 10)]

test_that("temperature", {
  input1Trans <- TransformTemp(input1, var = var, rounding = rounding)
  input2Trans <- TransformTemp(input2, var = var, rounding = rounding)

  expect_true(all(input1Trans[, c(1, 10)] == input2Trans))
})

## -----------------------------------------------------------------------------
## radiation
## -----------------------------------------------------------------------------
input1     <- ReadInput("rsds",
    KnmiRefFile("KNMI14____ref_rsds___19810101-20101231_v3.2.txt"))

input2 <- input1
input2$header <- input2$header[, c(1, 10)]
input2$obs    <- input2$obs[, c(1, 10)]
input2$coords <- input2$coords[10 - 1, 1:2, drop = FALSE]

test_that("radiation", {
  input1Trans <- TransformRadiation(input1, rounding = rounding)
  input2Trans <- TransformRadiation(input2, rounding = rounding)

  expect_true(all(input1Trans[, c(1, 10)] == input2Trans))
})
