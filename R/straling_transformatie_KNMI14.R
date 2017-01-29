#' Transformation of radiation
#' @description Function reads 'reference data' with time series of daily global
#'  radiation sums [kJ/m2] and 'change factors' from input files and applies
#'  them to function 'rsds_trans_KNMI14' to obtain 'future time series' that
#'  match a certain climate
#' @inheritParams temperatuur_transformatie_KNMI14
#' @param delta.file     [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory column identified with compulsory headers
#'                - HEADER -
#'                "ave"       relative change [\%] in mean shortwave surface radiation
#' @note The 5th row of the ifile indicated by 00000000 must be given as it is
#' interpreted to contain LATITUDES of station.
#' @export
straling_transformatie_KNMI14 <- function(ifile,
                                          ofile=NA,
                                          delta.file = NA,
                                          sc,
                                          p=NA) {

  version <- ReturnPackageVersion()

  CheckPeriod(p)

  input <- ReadInput("rsds", ifile)

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(delta.file, "rsds", sc, p)

  # TRANSFORMATION
  fut <- rsds_trans_KNMI14(obs=input$obs, deltas=deltas, lat=input$lat)

  # OUTPUT #####################################################################
  fut <- as.data.table(fut)
  result <- rbind(input$header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("rsds", ofile, version, sc, p, input$comments, result)
  }

  flog.debug("Radiation transformation ended successfully!")
  flog.debug("")
  return(result)
}


