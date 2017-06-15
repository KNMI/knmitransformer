library(testthat)
library(knmitransformer)

test_check("knmitransformer", filter = "readInput")
test_check("knmitransformer", filter = "SingleStations")
