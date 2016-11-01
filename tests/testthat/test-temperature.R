context("Temperature transformation")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('knmitransformer.log'))
library(data.table)

context("tg transformation - Entire station set")

var="tg"
ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2.txt"
ofile      <- "tmp.txt" # output file - used only temporary
delta.file <- NA
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  sc="GL"

  p=2030
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tg.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tg.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tg.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tg.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tg.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tg.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tg.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tg.rds")
})

context("tx transformation - Entire station set")

var="tx"
ifile="regressionInput/temperature/KNMI14____ref_tx___19810101-20101231_v3.2.txt"
ofile      <- "tmp.txt" # output file - used only temporary
delta.file <- NA
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  sc="GL"

  p=2030
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx.rds")
})

context("tn transformation - Entire station set")

var="tn"
ifile="regressionInput/temperature/KNMI14____ref_tn___19810101-20101231_v3.2.txt"
ofile      <- "tmp.txt" # output file - used only temporary
delta.file <- NA
regio.file <- "stationstabel" # table that links stations to region

test_that("2030 decadal prediction", {
  sc="GL"

  p=2030
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tn.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tn.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tn.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tn.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tn.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tn.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tn.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tn.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tn.rds")
})



context("Temperature transformation - Single station exercises")

test_that("Test wrong user input", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"     # input file
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  delta.file=NA            # file containing deltas
  sc="GL"                    # scenario ("GL", "GH", "WL", "WH")
  p=2030                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  expect_error(temperatuur_transformatie_KNMI14(ifile=ifile,
                                   ofile=ofile,
                                   delta.file=delta.file,
                                   sc="GL",
                                   p=NA,
                                   var=var
                                   ),
               "Period must be valid, i.e. 2030, 2050, or 2085")

})


test_that("Procedure works for default region", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"
  sc="GL"
  p=2030
  var="tg"

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg_DefaultRegion.rds")
})

test_that("Procedure works for one station as well", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"
  sc="GL"
  p=2030
  var="tg"

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) WH", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"     # input file
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  delta.file=NA            # file containing deltas
  # if delta.file is not provided (DEFAULT)
  # KNMI'14 deltas are used

  # deltas are derived from KNMI'14 deltas if delta.file is not specified
  # following arguments are used
  sc="WH"                    # scenario ("GL", "GH", "WL", "WH")
  p=2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tg___DeBilt.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) GL", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"     # input file
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  delta.file=NA            # file containing deltas
  # if delta.file is not provided (DEFAULT)
  # KNMI'14 deltas are used

  # deltas are derived from KNMI'14 deltas if delta.file is not specified
  # following arguments are used
  sc="GL"                    # scenario ("GL", "GH", "WL", "WH")
  p=2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tg___DeBilt.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) GH", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"     # input file
  ofile="uitvoer_DeBilt_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  delta.file=NA            # file containing deltas
  # if delta.file is not provided (DEFAULT)
  # KNMI'14 deltas are used

  # deltas are derived from KNMI'14 deltas if delta.file is not specified
  # following arguments are used
  sc="GH"                    # scenario ("GL", "GH", "WL", "WH")
  p=2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tg___DeBilt.rds")

  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tg___DeBilt.rds")
})

test_that("Temperature regression test (with actual data) WL", {
  ifile="regressionInput/temperature/KNMI14____ref_tg___19810101-20101231_v3.2_260.txt"     # input file
  ofile="uitvoer_tg.txt"      # output file (DEFAULT="uitvoer.txt")
  delta.file=NA            # file containing deltas
  # if delta.file is not provided (DEFAULT)
  # KNMI'14 deltas are used

  # deltas are derived from KNMI'14 deltas if delta.file is not specified
  # following arguments are used
  sc="WL"                    # scenario ("GL", "GH", "WL", "WH")
  p=2050                     # time horizon (2030, 2050, 2085)
  var="tg"                   # temperature characteristic ("tg" = mean,
  #                             "tn" = min,
  #                             "tx" = max)
  regio.file="stationstabel" # table that links stations to region

  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tg___DeBilt.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios - on the web there is an extended version
  p = 2085
  tmp <- temperatuur_transformatie_KNMI14(ifile=ifile,
                                          ofile=ofile,
                                          delta.file=delta.file,
                                          sc=sc,
                                          p=p,
                                          var=var,
                                          regio.file=regio.file)
  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tg___DeBilt.rds")
})
