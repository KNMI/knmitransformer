context("SingleStations")

## TODO: Rename context
## TODO: Add more tests

input1 <- ReadInput("rr", system.file("refdata",
                            "KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt",
                            package="knmitransformer"))

input2 <- input1
input2$header <- input2$header[, c(1,10)]
input2$obs    <- input2$obs[, c(1,10)]

test_that("precipitation", {
  input1Trans <- TransformPrecip(input1, ofile = NA, scenario = "GL")
  input2Trans <- TransformPrecip(input2, ofile = NA, scenario = "GL")

  expect_true(all(input1Trans[, c(1,10)] == input2Trans))
})
