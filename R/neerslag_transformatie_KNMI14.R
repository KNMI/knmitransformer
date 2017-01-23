#' Transformation of precipitation
#' @description Function reads 'reference data' with daily precipitation sums
#' [mm] and 'change factors' from input files and applies them to function
#' 'rr_trans_KNMI14' to obtain 'future time series' that match a certain climate
#' @inheritParams temperatuur_transformatie_KNMI14
#' @param delta.file     [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory columns identified with compulsory headers
#'                "wdf"       relative change in wet-day frequency
#'                            (wet days are defined as days with 0.1 mm or more precipitation) \cr
#'                "ave"       relative change in mean precipitation \cr
#'                "p99.lower" lower   estimate of the relative change in the 99th percentile of wet-day amounts \cr
#'                "p99.centr" central estimate of the relative change in the 99th percentile of wet-day amounts \cr
#'                "p99.upper" upper   estimate of the relative change in the 99th percentile of wet-day amounts \cr
#'
#'                 (If delta.file is not provided, predefined deltas are derived dependening on <sc>, <p> and <scaling>)
#' @param scaling        scaling extreme precipitation ["lower", "centr" (=DEFAULT), "upper"]
#' @param dryingScheme "v1.1" [DEFAULT] official version that belongs to KNMI'14
#                "v1.2" alternative procedure to dry wet days
#' @export
neerslag_transformatie_KNMI14 <- function(ifile,
                                          ofile = NA,
                                          delta.file = NA,
                                          sc,
                                          p = 2030,
                                          scaling = "centr",
                                          dryingScheme = "v1.1") {

  flog.info("Running temperature transformation")
  version <- packageVersion("knmitransformer")
  flog.debug("Version={%s}", version)
  flog.debug("DryingScheme={%s}", dryingScheme)

  CheckPeriod(p)

  # READ REFERENCE DATA FROM ifile
  input <- ReadInput("rr", ifile)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(delta.file, "rr", sc, p, scaling)

  # TRANSFORMATION
  fut <- rr_trans_KNMI14(obs = input$obs, deltas = deltas,
                         dryingScheme = dryingScheme)

  # OUTPUT
  fut <- as.data.table(fut)
  result <- rbind(input$header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("rr", ofile, version, sc, p, input$comments, result,
                scaling = scaling, dryingScheme = dryingScheme)
  }

  flog.debug("Precipitation transformation ended successfully!")
  flog.debug("")
  return(result)
}












