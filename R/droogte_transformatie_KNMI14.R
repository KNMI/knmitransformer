#' Calculation of Makkink evaporation
#' @description Function reads transormed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate
#' @inheritParams TransformTemp
#' @param ifile_tg   Name of the input file for temperature
#' @param ifile_rsds Name of the input file for radiation
#' @export
TransformEvap <- function(ifile_tg, ifile_rsds,
                          ofile = NA,
                          sc,
                          horizon = NA,
                          regio.file = NA) {

  version <- ReturnPackageVersion()

  CheckPeriod(p)

  rsds_input <- TransformRadiation(ifile = ifile_rsds, sc=sc, horizon = horizon)
  tg_input   <- TransformTemp(ifile = ifile_tg, var="tg", sc=sc,
                              horizon = horizon, regio.file = regio.file)

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
    WriteOutput("evmk", ofile, version, sc, horizon, H.comments, result)
  }

  flog.debug("Evaporation calculation ended successfully!")
  flog.debug("")
  return(result)
}

