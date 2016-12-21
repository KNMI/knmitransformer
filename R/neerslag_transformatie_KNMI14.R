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
                                          ofile = "uitvoer.txt",
                                          delta.file = NA,
                                          sc,
                                          p = 2030,
                                          scaling = "centr",
                                          dryingScheme = "v1.1") {

  flog.info("Running temperature transformation")
  version <- packageVersion("knmitransformer")
  flog.debug("Version={%s}", version)
  flog.debug("DryingScheme={%s}", dryingScheme)

  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
    stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }

  # READ REFERENCE DATA FROM ifile
  flog.info("Reading reference data, file={%s}", ifile)
  H.comments <- scan(ifile, character(0), sep = "\n", quiet=TRUE) # select lines with "#" from reference file and ignore them
  H.comments <- H.comments[grep("#",H.comments)]      # (only necessary for output file)

  obs        <- read.table(ifile,header=F)            # read reference data (header wordt niet apart ingelezen)
  header     <- obs[which(obs[,1]==0),]               # header met stations meta-data etc.
  header[,1] <- "00000000"
  names(obs) <- c("date",round(obs[1,-1],0))          # station names are read from first line
  obs        <- obs[which(obs[,1]!=0),]               # actual data

  # READ CHANGE FACTORS (DELTAS)
  deltas <- ReadChangeFactors(delta.file, "rr", sc, p, scaling)

  # TRANSFORMATION
  fut <- rr_trans_KNMI14(obs = obs, deltas = deltas,
                         dryingScheme = dryingScheme)

  # OUTPUT
  result <- WriteOutput("rr", ofile, version, sc, p, H.comments, header, fut,
                        scaling, dryingScheme = dryingScheme)
  flog.debug("Precipitation transformation ended successfully!")
  flog.debug("")
  return(result)
}












