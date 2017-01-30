#' Transformation of precipitation
#' @description Function reads 'reference data' with daily precipitation sums
#' [mm] and 'change factors' from input files and applies them to function
#' 'rr_trans_KNMI14' to obtain 'future time series' that match a certain climate
#' @inheritParams TransformTemp
#' @param subscenario  subscenario for extreme precipitation ["lower", "centr" (=DEFAULT), "upper"]
#' @param dryingScheme "v1.1" [DEFAULT] official version that belongs to KNMI'14
#                "v1.2" alternative procedure to dry wet days
#' @export
TransformPrecip <- function(ifile,
                            ofile = NA,
                            sc,
                            horizon = 2030,
                            subscenario = "centr",
                            dryingScheme = "v1.1") {

  version <- ReturnPackageVersion()
  flog.debug("DryingScheme={%s}", dryingScheme)

  CheckPeriod(horizon)

  # READ REFERENCE DATA FROM ifile
  input <- ReadInput("rr", ifile)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(NA, "rr", sc, horizon, subscenario)

  # TRANSFORMATION
  fut <- rr_trans_KNMI14(obs = input$obs, deltas = deltas,
                         dryingScheme = dryingScheme)

  # OUTPUT
  fut <- as.data.table(fut)
  result <- rbind(input$header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("rr", ofile, version, sc, horizon, input$comments, result,
                subscenario = subscenario, dryingScheme = dryingScheme)
  }

  flog.debug("Precipitation transformation ended successfully!")
  flog.debug("")
  return(result)
}












