#' Transformation of temperature
#' @description Function reads 'reference data' with ime series of daily of
#'  mean, minimum or maximum temperature [degrees Celsius] and 'change factors'
#'  from input files and applies them to function 'tm_trans_KNMI14' to obtain
#'  'future time series' that match a certain climate
#' @param ifile          Name of the input file (ASCII) that contains reference data (all numerics) in which
#'                the columns provide time series for specific stations. The first column
#'                should provide either 00000000 or a datestring YYYYMMDD:
#'                Rows starting with 00000000 are considered station info (station number, lat, lon
#'                etc.) and are ignored.
#'                Rows starting with a datestring refer to a specific day in the time series.
#'                Rows starting with "#" are completely ignored and returned unchanged
#'
#' @param ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                Format is similar to ifile
#'
#' @param delta.file     [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory columns identified with compulsory headers
#'               - HEADER -
#'               "maand"     month for which deltas are valid (1,2,...,12)
#'               "P01"       1st  percentile daily temperature
#'               "P05"       5th  percentile daily temperature
#'               "P50"       50th percentile daily temperature
#'               "P95"       95th percentile daily temperature
#'               "P99"       99th percentile daily temperature
#'
#'               following column is optional in case deltas vary with region
#'               (is needed in case <regio.tabel> is provided)
#'               "regio"     region for which deltas are valid
#'                           KNMI14 distinguishes ("NWN", "ZWN", "NON", "MON", "ZON", "NLD")
#'
#'                 (If delta.file is not provided, predefined deltas are derived dependening on <sc>, <p> and
#'                  daily temperature characteristic of interest [mean, min or max])
#'
#' @param sc             scenario                      ["GL", "GH", "WL", "WH"]
#' @param p              time horizon                  [2030 (=DEFAULT), 2050, 2085]
#' @param var            kind of daily temperature variable ["tg" = daily mean,
#'                                                    "tn" = daily minimum,
#'                                                    "tx" = daily maximum]
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
temperatuur_transformatie_KNMI14 <- function(ifile,
                                              ofile="uitvoer.txt",
                                              delta.file=NA,
                                              sc,
                                              p=2030,
                                              var,
                                              regio.file=NA) {
  flog.info("Running temperature transformation")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ###############################################################################
  version="v1.0"

  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
    stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }

  # READ REFERENCE DATA FROM ifile
  flog.info("Reading reference data, file={%s}", ifile)
  H.comments <- scan(ifile, character(0), sep = "\n", quiet=TRUE) # select lines with "#" from reference file and ignore them
  flog.debug("Scanning of the reference data returned n={%i} lines.", length(H.comments))
  H.comments <- H.comments[grep("#",H.comments)]      # (only necessary for output file)

  obs        <- read.table(ifile,header=F)            # read reference data (header wordt niet apart ingelezen)
  header     <- obs[which(obs[,1]==0),]               # header met stations meta-data etc.
  header[,1] <- "00000000"
  names(obs) <- c("date",round(obs[1,-1],0))          # station names are read from first line
  obs        <- obs[which(obs[,1]!=0),]               # actual data

  flog.debug("obs colnames={%s}", paste(names(obs), collapse = ", "))
  #print(obs[1:10,])

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(delta.file, var, sc, p)

  #print(deltas)

  # LINK STATIONS TO REGIONS
  if(is.na(regio.file)) {
    regio.tabel <- NA
  } else {
    tmpPath <- system.file("extdata", "stationstabel", package="knmitransformer")
    stationstabel <- read.table(tmpPath)
    regio.tabel   <- as.vector(stationstabel[match(names(obs)[-1],stationstabel[,1]),2])
  }

  flog.debug("regio.table={%s}", paste(regio.tabel, collapse = ", "))

  # TRANSFORMATION
  #source("tm_trans_KNMI14.R")
  fut <- tm_trans_KNMI14(obs=obs, deltas=deltas, regio.tabel=regio.tabel)

  # OUTPUT #############################################################################################
  result <- WriteOutput(var, ofile, version, sc, p, H.comments, header, fut)

  flog.debug("Temperature transformation ended successfully!")
  flog.debug("")
  return(result)
} # end function temp.trans_KNMI14
