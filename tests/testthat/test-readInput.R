context("readInput")

## TODO: Rename context
## TODO: Add more tests

test_that("read input works correctly", {
  expect_error(ReadInput("rsds", "./regressionInput/radiation/KNMI14____ref_rsds___19810101-20101231_wrong.txt"),
               "Check input file: Latitude NOT (or not properly) provided.",
               fixed=TRUE)
})
