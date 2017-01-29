#' Calculation of Makkink evaporation
#' @description Function reads transormed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate
#' @inheritParams temperatuur_transformatie_KNMI14
#' @param ifile_tg   Name of the input file for temperature
#' @param ifile_rsds Name of the input file for radiation
#' @param regio.file     this (optional) argument provides the name of an ASCII file that relates the stations to
#'                a particular region. First column is station id and second column region
#'                KNMI14 distinguishes following regions:
#'                <NLD> Nederland            [DEFAULT]
#'                <NWN> Noordwest Nederland
#'                <ZWN> Zuidwest Nederland
#'                <NON> Noordoost Nederland
#'                <MON> Middenoost Nederland
#'                <ZON> Zuidoost Nederland
#'
#' @export
droogte_berekening_KNMI14 <- function(ifile_tg, ifile_rsds,
                                      ofile=NA,
                                      sc,
                                      p=NA,
                                      regio.file = NA) {

  version <- ReturnPackageVersion()
  # CONSTANTS AND FUNCTIONS ###############################################################################

  CheckPeriod(p)

  ## Need to add an IF for delta.files of rsds & tg = need to be for the same p and the same sc

  rsds_input <- straling_transformatie_KNMI14(ifile = ifile_rsds, sc=sc, p=p)
  tg_input   <- temperatuur_transformatie_KNMI14(ifile = ifile_tg, var="tg", sc=sc, p=p, regio.file = regio.file)

  rsds <- rsds_input[-(1:5)]
  tg   <- tg_input[-(1:5)]
  if (!all(rsds_input[1:5] == tg_input[1:5])) {
    flog.error("Same stations should be used for temperature and radiation")
    stop("Same stations should be used for temperature and radiation")
  }

  fut               <- rsds
  fut[,2:ncol(fut)] <- NA
  fut[,2:ncol(fut)] <- round(makkink(tg[,2:ncol(fut),with=FALSE],rsds[,2:ncol(fut),with=FALSE]),2)

  # Have to add a test to make sure that the header here is the same as the header in the regressionInput files
  header     <- rsds_input[1:5]
  H.comments <- "# Makkink Evaporation [mm] as derived from transformed tg & rsds "

  # OUTPUT #####################################################################
  fut <- as.data.table(fut)
  result <- rbind(header, fut, use.names = FALSE)
  result[, V1 := as.integer(V1)]

  if (!is.na(ofile)) {
    WriteOutput("evmk", ofile, version, sc, p, H.comments, result)
  }

  flog.debug("Evaporation calculation ended successfully!")
  flog.debug("")
  return(result)
}

