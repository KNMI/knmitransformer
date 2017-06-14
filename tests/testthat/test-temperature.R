context("Temperature transformation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer_temperature.log'))
library(data.table)

context("tg transformation - Entire station set")

var        <- "tg"
input      <- system.file("refdata",
                          "KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                          package="knmitransformer")
ofile      <- NA
regio.file <- "stationstabel" # table that links stations to region
rounding   <- FALSE

test_that("2030 decadal prediction", {
  scenario="GL"

  horizon = 2030
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
                       )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg.rds")

  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=TRUE)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg_rounded.rds")
})

test_that("Scenario WL", {
  scenario="WL"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tg.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tg.rds")
})

test_that("Scenario WH", {
  scenario="WH"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tg.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tg.rds")
})

test_that("Scenario GH", {
  scenario="GH"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tg.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tg.rds")
})

test_that("Scenario GL", {
  scenario="GL"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tg.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tg.rds")
})

# ------------------------------------------------------------------------------
context("tx transformation - Entire station set")

var        <- "tx"
input      <- system.file("refdata",
                          "KNMI14____ref_tx___19810101-20101231_v3.2.txt",
                          package="knmitransformer")
ofile      <- NA # output file - used only temporary
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  scenario="GL"

  horizon = 2030
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx.rds")
})

test_that("Scenario WL", {
  scenario="WL"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx.rds")
})

test_that("Scenario WH", {
  scenario="WH"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx.rds")
})

test_that("Scenario GH", {
  scenario="GH"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx.rds")
})

test_that("Scenario GL", {
  scenario="GL"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx.rds")
})

context("tn transformation - Entire station set")

var        <- "tn"
input      <- system.file("refdata",
                          "KNMI14____ref_tn___19810101-20101231_v3.2.txt",
                          package="knmitransformer")
ofile      <- NA # output file - used only temporary
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  scenario="GL"

  horizon = 2030
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tn.rds")
})

test_that("Scenario WL", {
  scenario="WL"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tn.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tn.rds")
})

test_that("Scenario WH", {
  scenario="WH"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tn.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tn.rds")
})

test_that("Scenario GH", {
  scenario="GH"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tn.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tn.rds")
})

test_that("Scenario GL", {
  scenario="GL"

  horizon = 2050
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tn.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       regio.file=regio.file,
                       rounding=rounding
  )
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tn.rds")
})



context("Temperature transformation - Single station exercises")
input <- "regressionInput/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"

test_that("Test wrong user input", {
  ofile    = "uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  scenario = "GL"                    # scenario ("GL", "GH", "WL", "WH")
  horizon  = 2030                     # time horizon (2030, 2050, 2085)
  var      = "tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  expect_error(TransformTemp(input=input,
                             ofile=ofile,
                             scenario="GL",
                             horizon = NA,
                             var=var,
                             rounding = rounding
                             ),
               "Period must be valid, i.e. 2030, 2050, or 2085")

  expect_error(TransformTemp(input=input,
                             ofile=ofile,
                             scenario="GL",
                             horizon = horizon,
                             var="blub",
                             rounding = rounding
                             ),
               "variable not defined.")

})


test_that("Procedure works for default region", {
  scenario="GL"
  horizon = 2030
  var="tg"

  tmp <- TransformTemp(input=input,
                       ofile=ofile,
                       scenario=scenario,
                       horizon = horizon,
                       var=var,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg_DefaultRegion.rds")
})

test_that("Procedure works for one station as well", {
  scenario="GL"
  horizon = 2030
  var="tg"

  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) WH", {
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")

  scenario="WH"                    # scenario ("GL", "GH", "WL", "WH")
  horizon = 2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tg___DeBilt.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) GL", {
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")

  scenario="GL"                    # scenario ("GL", "GH", "WL", "WH")
  horizon = 2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tg___DeBilt.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) GH", {
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  scenario="GH"                    # scenario ("GL", "GH", "WL", "WH")
  horizon = 2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tg___DeBilt.rds")

  horizon = 2085
  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) WL", {
  ofile="uitvoer_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  scenario="WL"                    # scenario ("GL", "GH", "WL", "WH")
  horizon = 2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tg___DeBilt.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  horizon = 2085
  tmp <- TransformTemp(input=input,
                                          ofile=ofile,
                                          scenario=scenario,
                                          horizon = horizon,
                                          var=var,
                                          regio.file=regio.file,
                       rounding = rounding)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tg___DeBilt.rds")
})
