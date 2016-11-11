#' Calculation of Makkink evaporation
#' @description Function reads transormed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate
#' @param ifile          Name of the input file (ASCII) that contains reference data (all numerics) in which
#'                the columns provide transformed time series for specific stations. The first column
#'                should provide either 00000000 or a datestring YYYYMMDD:
#'                Rows starting with 00000000 are considered station info (station number, lat, lon
#'                etc.) and are ignored.
#'                Rows starting with a datestring refer to a specific day in the time series.
#'                Rows starting with "#" are completely ignored and returned unchanged
#'
#' @param ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                Format is similar to ifile
#'
#' @param sc             scenario                      ["GL", "GH", "WL", "WH"]
#' @param p              time horizon                  [2030 (=DEFAULT), 2050, 2085]
#' @param var          daily Makkink evaporation [evmk= evaporation makkink]
#'
#'@param delta.file.rsds     [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory column identified with compulsory headers
#'                - HEADER -
#'                "ave"       relative change [\%] in mean shortwave surface radiation
#'@param delta.file.tg    [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory columns identified with compulsory headers
#'               - HEADER -
#'               "maand"     month for which deltas are valid (1,2,...,12) \cr
#'               "P01"       1st  percentile daily temperature \cr
#'               "P05"       5th  percentile daily temperature \cr
#'               "P50"       50th percentile daily temperature \cr
#'               "P95"       95th percentile daily temperature \cr
              # "P99"       99th percentile daily temperature \cr
#'
#'               following column is optional in case deltas vary with region
#'               (is needed in case <regio.tabel> is provided)
#'               "regio"     region for which deltas are valid
#'                           KNMI14 distinguishes ("NWN", "ZWN", "NON", "MON", "ZON", "NLD")
#'
#'                 (If delta.file is not provided, predefined deltas are derived dependening on <sc>, <p> and
#'                  daily temperature characteristic of interest [mean, min or max])
#'
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
#' @importFrom data.table fread
#' @export


droogte_berekening_KNMI14 <- function(ifile_tg, ifile_rsds,
                                      ofile="uitvoer.txt",
                                      delta.file.rsds,
                                      delta.file.tg = NA,
                                      sc,
                                      p=NA,
                                      regio.file = NA) {

  flog.info("Running evaporation calculation")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ###############################################################################
  version="v1.0"

  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
    stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }

  ## Need to add an IF for delta.files of rsds & tg = need to be for the same p and the same sc

  rsds_input <- straling_transformatie_KNMI14(ifile = ifile_rsds, delta.file = delta.file.rsds, p=2030)
  tg_input <- temperatuur_transformatie_KNMI14(ifile = ifile_tg, var="tg" , delta.file = delta.file.tg)


      #rsds_input        <- rsds_input[-h.ids,]
      #tg          <-   tg[-h.ids,]

      #ns         <- ncol(rsds_input) - 1
      #dt         <- rsds_input[,1]
      #nr         <- length(dt)

      #ev         <- rsds_input; ev[,-1] = NA
      #ev[,-1]    <- round(makkink(tg[,-1],1000*rsds_input[,-1]),2)
      fut    <- round(makkink(tg_input,1000*rsds_input),2)

      # OUTPUT #####################################################################
      result <- WriteOutput("evmk", ofile, version, sc, p, H.comments, header, fut)

      flog.debug("Evaporation calculation ended successfully!")
      flog.debug("")
      return(result)
}

