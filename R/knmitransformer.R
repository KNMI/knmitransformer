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
#' @param rounding Logical (default = TRUE) if results should be rounded
#' @export
TransformTemp <- function(ifile,
                          ofile = NA,
                          scenario,
                          horizon = 2030,
                          var,
                          regio.file = NA,
                          rounding = TRUE) {
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
  if(rounding) {
    fut[, -1] <- round(fut[, -1], 1)
  }
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

#' Show station table
#' @export
ShowStationTable <- function() {
  tmp <- system.file("extdata", "stationstabel",
                     package = "knmitransformer")
  system(paste0("less ", tmp))
}

#' Transformation of radiation
#' @description Function reads 'reference data' with time series of daily global
#'  radiation sums `[kJ/m2]` and 'change factors' from input files and applies
#'  them to function 'rsds_trans_KNMI14' to obtain 'future time series' that
#'  match a certain climate
#' @inheritParams TransformTemp
#' @note The 5th row of the ifile indicated by 00000000 must be given as it is
#' interpreted to contain LATITUDES of station.
#' @export
TransformRadiation <- function(ifile,
                               ofile=NA,
                               scenario,
                               horizon = 2030,
                               rounding = TRUE) {

  version <- ReturnPackageVersion("rsds")

  CheckPeriod(horizon)

  input <- ReadInput("rsds", ifile)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors("rsds", scenario, horizon)

  # TRANSFORMATION
  fut <- rsds_trans_KNMI14(obs=input$obs, deltas=deltas, lat=input$lat)

  # OUTPUT #####################################################################
  if(rounding) {
    fut[, -1] <- round(fut[, -1])
  }
  fut <- as.data.table(fut)
  result <- rbind(input$header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("rsds", ofile, version, scenario, horizon, input$comments, result)
  }

  flog.debug("Radiation transformation ended successfully!")
  flog.debug("")
  return(result)
}

#' Transformation of precipitation
#' @description Function reads 'reference data' with daily precipitation sums
#' `[mm]` and 'change factors' from input files and applies them to function
#' 'rr_trans_KNMI14' to obtain 'future time series' that match a certain climate
#' @inheritParams TransformTemp
#' @param subscenario  subscenario for extreme precipitation
#' ("lower", "centr" (=DEFAULT), "upper")
#' @export
TransformPrecip <- function(ifile,
                            ofile = NA,
                            scenario,
                            horizon = 2030,
                            subscenario = "centr",
                            rounding = TRUE) {

  version <- ReturnPackageVersion("rr")
  dryingScheme = "v1.1"
  flog.debug("DryingScheme={%s}", dryingScheme)

  CheckPeriod(horizon)

  # READ REFERENCE DATA FROM ifile
  input <- ReadInput("rr", ifile)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors("rr", scenario, horizon, subscenario)

  # TRANSFORMATION
  fut <- rr_trans_KNMI14(obs = input$obs, deltas = deltas)

  # OUTPUT
  if(rounding) {
    fut[, -1] <- round(fut[, -1], 1)
  }
  fut <- as.data.table(fut)
  result <- rbind(input$header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("rr", ofile, version, scenario, horizon, input$comments, result,
                subscenario = subscenario)
  }

  flog.debug("Precipitation transformation ended successfully!")
  flog.debug("")
  return(result)
}

#' Calculation of Makkink evaporation
#' @description Function reads transformed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate
#' @inheritParams TransformTemp
#' @param ifile_tg   Name of the input file for temperature
#' @param ifile_rsds Name of the input file for radiation
#' @export
TransformEvap <- function(ifile_tg, ifile_rsds,
                          ofile = NA,
                          scenario,
                          horizon = 2030,
                          regio.file = NA,
                          rounding = TRUE) {

  version <- ReturnPackageVersion("evmk")

  CheckPeriod(horizon)

  rsds_input <- TransformRadiation(ifile = ifile_rsds, scenario=scenario, horizon = horizon,
                                   rounding = FALSE)
  tg_input   <- TransformTemp(ifile = ifile_tg, var="tg", scenario=scenario,
                              horizon = horizon, regio.file = regio.file,
                              rounding = FALSE)

  rsds <- rsds_input[-(1:5), ]
  tg   <- tg_input[-(1:5), ]
  if (!all(rsds_input[1:5, ] == tg_input[1:5, ])) {
    flog.error("Same stations should be used for temperature and radiation")
    stop("Same stations should be used for temperature and radiation")
  }

  fut               <- rsds
  fut[,2:ncol(fut)] <- NA
  fut[,2:ncol(fut)] <- makkink(tg[,2:ncol(fut),with=FALSE],rsds[,2:ncol(fut),with=FALSE])

  # Have to add a test to make sure that the header here is the same as the header in the regressionInput files
  header     <- rsds_input[1:5, ]
  H.comments <- "# Makkink Evaporation [mm] as derived from transformed tg & rsds "

  # OUTPUT #####################################################################
  if(rounding) {
    fut <- as.data.frame(fut)
    fut[, -1] <- round(fut[, -1], 2)
  }
  fut <- as.data.table(fut)
  result <- rbind(header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("evmk", ofile, version, scenario, horizon, H.comments, result)
  }

  flog.debug("Evaporation calculation ended successfully!")
  flog.debug("")
  return(result)
}
