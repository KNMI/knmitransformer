#' Transformation of temperature
#' @description Function reads 'reference data' with time series of daily of
#'  mean, minimum or maximum temperature `[degree Celsius]` and 'change factors'
#'  from input files and applies them to function 'tm_trans_KNMI14' to obtain
#'  'future time series' that match a certain climate
#' @param input  knmiTF object or name of the input file (ASCII) that contains
#'  reference data (all numerics) in which the columns provide time series for
#'  specific stations. The first column should provide either 00000000 or a
#'  datestring YYYYMMDD: \cr
#'  Rows starting with 00000000 are considered station info (station number,
#'  lat, lon etc.) and are ignored. \cr
#'  Rows starting with a datestring refer to a specific day in the time series. \cr
#'  Rows starting with "#" are completely ignored and returned
#'  unchanged.
#'
#' @param ofile Name of the output file to write the transformed data to.
#'   Format is similar to input file mentioned in input. By default no output
#'   file is written.
#' @param scenario scenario ("GL"=DEFAULT, "GH", "WL", "WH")
#' @param horizon  time horizon (2030=DEFAULT, 2050, 2085)
#' @param var    kind of daily temperature variable
#'               ("tg" = daily mean, "tn" = daily minimum, "tx" = daily maximum)
#'
#' @param regions vector of regions
#'                KNMI14 distinguishes following regions:\cr
#'                <NLD> Nederland (DEFAULT) \cr
#'                <NWN> Noordwest Nederland \cr
#'                <ZWN> Zuidwest Nederland \cr
#'                <NON> Noordoost Nederland \cr
#'                <MON> Middenoost Nederland \cr
#'                <ZON> Zuidoost Nederland
#' @param rounding Logical (default = TRUE) if results should be rounded
#' @export
TransformTemp <- function(input,
                          var,
                          scenario = "GL",
                          horizon = 2030,
                          regions = "NLD",
                          ofile = NA,
                          rounding = TRUE) {
  version <- ReturnPackageVersion(var)

  # CONSTANTS AND FUNCTIONS ####################################################

  CheckPeriod(horizon)

  userProvided <- CheckIfUserProvided(input)
  if (class(input) != "knmiTF") {
    input <- ReadInput(var, input)
  }

  regions <- CheckRegions(regions, ncol(input$obs) - 1)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(var, scenario, horizon)

  flog.debug("regions={%s}", paste(regions, collapse = ", "))

  # TRANSFORMATION
  fut <- tm_trans_KNMI14(obs = input$obs, deltas = deltas,
                         regio.tabel = regions)

  result <- PrepareOutput(fut, var, input$header, rounding)

  if (!is.na(ofile)) {
    WriteOutput(var, ofile, version, scenario, horizon, input$comments, result,
                userProvided = userProvided)
  }

  flog.debug("Temperature transformation ended successfully!")
  flog.debug("")
  return(result)
} # end function temp.trans_KNMI14



#' Transformation of radiation
#' @description Function reads 'reference data' with time series of daily global
#'  radiation sums `[kJ/m2]` and 'change factors' from input files and applies
#'  them to function 'rsds_trans_KNMI14' to obtain 'future time series' that
#'  match a certain climate
#' @inheritParams TransformTemp
#' @note The 5th row of the ifile indicated by 00000000 must be given as it is
#' interpreted to contain LATITUDES of station.
#' @export
TransformRadiation <- function(input,
                               scenario = "GL",
                               horizon = 2030,
                               ofile=NA,
                               rounding = TRUE) {

  version <- ReturnPackageVersion("rsds")

  CheckPeriod(horizon)

  userProvided <- CheckIfUserProvided(input)
  if (class(input) != "knmiTF") {
    input <- ReadInput("rsds", input)
  }


  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors("rsds", scenario, horizon)

  # TRANSFORMATION
  fut <- rsds_trans_KNMI14(obs=input$obs, deltas=deltas, lat=input$lat)

  result <- PrepareOutput(fut, "rsds", input$header, rounding)

  if (!is.na(ofile)) {
    WriteOutput("rsds", ofile, version, scenario, horizon, input$comments,
                result, userProvided = userProvided)
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
TransformPrecip <- function(input,
                            scenario = "GL",
                            horizon = 2030,
                            subscenario = "centr",
                            ofile = NA,
                            rounding = TRUE) {

  version <- ReturnPackageVersion("rr")
  dryingScheme = "v1.1"
  flog.debug("DryingScheme={%s}", dryingScheme)

  CheckPeriod(horizon)

  # READ REFERENCE DATA FROM ifile
  userProvided <- CheckIfUserProvided(input)
  if (class(input) != "knmiTF") {
    input <- ReadInput("rr", input)
  }

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors("rr", scenario, horizon, subscenario)

  # TRANSFORMATION
  fut <- rr_trans_KNMI14(obs = input$obs, deltas = deltas)

  result <- PrepareOutput(fut, "rr", input$header, rounding)

  if (!is.na(ofile)) {
    WriteOutput("rr", ofile, version, scenario, horizon, input$comments, result,
                subscenario = subscenario, userProvided = userProvided)
  }

  flog.debug("Precipitation transformation ended successfully!")
  flog.debug("")
  return(result)
}

#' Calculation of Makkink evaporation
#' @description Function reads transformed mean temperature and transformed
#'   global radiation and calculates the Makkink evaporation for 'future time
#'   series' that match a certain climate
#' @inheritParams TransformTemp
#' @param input_tg   Name of the input file for temperature
#' @param input_rsds Name of the input file for radiation
#' @export
TransformEvap <- function(input_tg, input_rsds,
                          scenario = "GL",
                          horizon = 2030,
                          regions = "NLD",
                          ofile = NA,
                          rounding = TRUE) {

  version <- ReturnPackageVersion("evmk")

  CheckPeriod(horizon)

  userProvided <- CheckIfUserProvided(input_tg) |
    CheckIfUserProvided(input_rsds)


  rsds_input <- TransformRadiation(input = input_rsds, scenario=scenario,
                                   horizon = horizon,
                                   rounding = FALSE)
  tg_input   <- TransformTemp(input = input_tg, var="tg", scenario=scenario,
                              horizon = horizon, regions = regions,
                              rounding = FALSE)

  rsds <- rsds_input[-(1:5), ]
  tg   <- tg_input[-(1:5), ]
  if (!all(rsds_input[1:5, ] == tg_input[1:5, ])) {
    flog.error("Same stations should be used for temperature and radiation")
    stop("Same stations should be used for temperature and radiation")
  }

  fut               <- rsds
  fut[,2:ncol(fut)] <- NA
  fut[,2:ncol(fut)] <- makkink(tg[,2:ncol(fut),with=FALSE],
                               rsds[,2:ncol(fut),with=FALSE])

  # Have to add a test to make sure that the header here is the same as the
  # header in the regressionInput files
  header     <- rsds_input[1:5, ]
  H.comments <- "# Makkink Evaporation [mm] as derived from transformed tg & rsds "

  # OUTPUT #####################################################################

  result <- PrepareOutput(as.data.frame(fut), "evmk", header, rounding)

  if (!is.na(ofile)) {
    WriteOutput("evmk", ofile, version, scenario, horizon, H.comments, result,
                userProvided = userProvided)
  }

  flog.debug("Evaporation calculation ended successfully!")
  flog.debug("")
  return(result)
}
