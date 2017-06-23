context("helpers")

input <- KnmiRefFile("KNMI14____ref_rsds___19810101-20101231_v3.2.txt")
input <- ReadInput("rsds", input)

test_that("Obtain day number", {
  expect_equal_to_reference(ObtainDayNumber(input$obs[, 1]),
      "regressionOutput/additional/daynumber.rds")
})

test_that("Obtain day number", {
  expect_equal_to_reference(ObtainMonth(input$obs[, 1]),
      "regressionOutput/additional/month.rds")
})

test_that("KnmiRefFile works", {
  baseName <- "KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt"
  expect_equal(basename(KnmiRefFile(baseName)), baseName)
})
