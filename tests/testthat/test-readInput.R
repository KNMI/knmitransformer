context("readInput")

## TODO: Rename context
## TODO: Add more tests

test_that("read input works correctly", {
  expect_error(ReadInput("rsds", "./regressionInput/KNMI14____ref_rsds___19810101-20101231_wrong.txt"),
               "Check input file: Latitude NOT (or not properly) provided.", fixed = TRUE)
  expect_error(ReadInput("rr", "blah"),
               "Input file does not exist.")
})


context("createInput")

library(data.table)

filename <-  system.file("exampledata",
                         "tgtg260.dat",
                         package="knmitransformer")
tgData <- fread(filename, skip = 6)

tgData[, date := format(as.Date(paste(V1, V2, V3, sep = "-"), format = "%Y-%m-%d"), format = "%Y%m%d")]

tgData <- tgData[, .(date, tg = V4, year = V1)]


lat <- 52.1
lon <- 5.18
comment <- "# First one"

test_that("Input assertion", {
  expect_error(CreateKnmiTFInput(tgData, 1, lat, lon, comment),
               "Data should contain only column date and column values")
  expect_error(CreateKnmiTFInput(tgData[, .(date, tg)], 1, lat, lon, comment),
               "Data should contain only column date and column values")
  expect_error(CreateKnmiTFInput(tgData[, .(date, values = tg)], 1, lat, lon, comment),
               "Date col should contain date in integer format 'yyyymmdd'")
  tgData[, date := as.integer(date)]
  expect_error(CreateKnmiTFInput(tgData[year == 2009, .(date, values = tg)], 1, lat, lon, comment),
               "The date column should contain the full period from 19810101 to 20101231")
  expect_error(CreateKnmiTFInput(tgData[, .(date, values = tg)], 1, lat, lon, comment),
               "Currently the period is limited to the official thirty year period 1981-2010")
  tgData <- tgData[year %in% 1981 : 2010]
  expect_error(CreateKnmiTFInput(tgData[, .(date, values = tg)], 1, rep(lat, 2), lon, comment),
               "lat and lon should be of length 1")
  expect_error(CreateKnmiTFInput(tgData[, .(date, values = as.character(tg))], 1, rep(lat, 2), lon, comment),
               "Values should be of class numeric")
  expect_equal_to_reference(CreateKnmiTFInput(tgData[, .(date, values = tg)], 1, lat, lon, comment),
                            file = "regressionOutput/additional/CreateInput.rds")
})

test_that("Recognizes official files", {
  expect_true(CheckIfUserProvided("ab"))
  expect_false(CheckIfUserProvided(system.file("refdata",
                                               "KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                                               package="knmitransformer")))
})


