#' Transformation of radiation
#' @inheritParams temperatuur_transformatie_KNMI14
#' @param delta.file     [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory column identified with compulsory headers
#'                - HEADER -
#'                "ave"       relative change [\%] in mean shortwave surface radiation
#' @note The 5th row of the ifile indicated by 00000000 must be given as it is
#' interpreted to contain LATITUDES of station.
#' @export
straling_transformatie_KNMI14 <- function(ifile,
                                          ofile="uitvoer.txt",
                                          delta.file,
                                          sc,
                                          p=NA) {

  flog.info("Running temperature transformation")
  # CONSTANTS AND FUNCTIONS ####################################################
  version="v1.1"
  flog.debug("Version={%s}", version)

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

  lat        <- as.numeric(header[5,-1])              # lat; needed for calculating radiation TOA
                                                      # Expected as the fifth line of the header (comment added 20150601_JB)
  if (length(which(is.na(lat) == TRUE)) > 0) {
    flog.error("Check input file: Latitude NOT (or not properly) provided.")
    stop()
  }

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(delta.file, "rsds", sc, p)

  # TRANSFORMATION
  fut <- rsds_trans_KNMI14(obs=obs, deltas=deltas, lat=lat)

  # OUTPUT #####################################################################
  result <- WriteOutput("rsds", ofile, version, sc, p, H.comments, header, fut)

  flog.debug("Radiation transformation ended successfully!")
  flog.debug("")
  return(result)
}


