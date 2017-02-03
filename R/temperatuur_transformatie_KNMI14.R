#' Transformation of temperature
#' @description Function reads 'reference data' with time series of daily of
#'  mean, minimum or maximum temperature `[degree Celsius]` and 'change factors'
#'  from input files and applies them to function 'tm_trans_KNMI14' to obtain
#'  'future time series' that match a certain climate
#' @param ifile  Name of the input file (ASCII) that contains reference data
#'               (all numerics) in which the columns provide time series for
#'               specific stations.
#'               The first column should provide either 00000000 or a
#'               datestring YYYYMMDD:
#'               Rows starting with 00000000 are considered station info
#'               (station number, lat, lon etc.) and are ignored.
#'               Rows starting with a datestring refer to a specific day in the
#'               time series.
#'               Rows starting with "#" are completely ignored and returned
#'               unchanged.
#'
#' @param ofile  (DEFAULT=NA) Name of the output file to write the
#'               transformed data to.
#'               Format is similar to ifile
#' @param scenario scenario ("GL", "GH", "WL", "WH")
#' @param horizon  time horizon (2030=DEFAULT, 2050, 2085)
#' @param var    kind of daily temperature variable
#'               ("tg" = daily mean, "tn" = daily minimum, "tx" = daily maximum)
#'
#' @param regio.file this (optional) argument provides the name of an ASCII file
#'               that relates the stations to a particular region.
#'               First column is station id and second column region
#'                KNMI14 distinguishes following regions:
#'                <NLD> Nederland            (DEFAULT)
#'                <NWN> Noordwest Nederland
#'                <ZWN> Zuidwest Nederland
#'                <NON> Noordoost Nederland
#'                <MON> Middenoost Nederland
#'                <ZON> Zuidoost Nederland
#' @export
TransformTemp <- function(ifile,
                          ofile = NA,
                          scenario,
                          horizon = 2030,
                          var,
                          regio.file = NA) {
  version <- ReturnPackageVersion(var)

  # CONSTANTS AND FUNCTIONS ####################################################

  CheckPeriod(horizon)

  # READ REFERENCE DATA FROM ifile
  input <- ReadInput(var, ifile)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(var, scenario, horizon)

  # LINK STATIONS TO REGIONS
  if(is.na(regio.file)) {
    regio.tabel <- NA
  } else {
    tmpPath <- system.file("extdata", "stationstabel",
                           package = "knmitransformer")
    stationstabel <- read.table(tmpPath)
    regio.tabel   <- as.vector(stationstabel[match(names(input$obs)[-1],
                                                   stationstabel[, 1]), 2])
  }

  flog.debug("regio.table={%s}", paste(regio.tabel, collapse = ", "))

  # TRANSFORMATION
  fut <- tm_trans_KNMI14(obs = input$obs, deltas = deltas,
                         regio.tabel = regio.tabel)

  # OUTPUT #####################################################################
  fut <- as.data.table(fut)
  result <- rbind(input$header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput(var, ofile, version, scenario, horizon, input$comments, result)
  }

  flog.debug("Temperature transformation ended successfully!")
  flog.debug("")
  return(result)
} # end function temp.trans_KNMI14
