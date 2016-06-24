###########################################################################################################
#
# neerslag_transformatie_KNMI14.R    March 10, 2015
#
# author: Alexander Bakker (KNMI)
#
# Function reads 'reference data' with daily precipitation sums [mm] and
#                'change factors' from input files
# and applies them to function 'rr_trans_KNMI14' to obtain
# 'future time series' that match a certain climate
#
# Transformation developed for KNMI'14 climate change scenarios for the Netherlands:
#
# Bakker, A. (2015), Time series transformation tool: description of the program to generate time series
# consistent with the KNMI’14 climate scenarios, Technical Report TR-349, De Bilt, the Netherlands
#
#
# THIS CODE IS PROVIDED AS-IS WITH NO WARRANTY (NEITHER EXPLICIT
# NOT IMPLICIT).  KNMI SHARES THIS CODE IN HOPES THAT IT IS USEFUL,
# BUT KNMI IS NOT LIABLE FOR THE BEHAVIOR OF THIS CODE IN YOUR OWN
# APPLICATION. YOU ARE FREE TO SHARE THIS CODE SO LONG AS THE
# AUTHOR(S) AND VERSION HISTORY REMAIN INTACT.
#
#
# arguments:
#
# ifile          Name of the input file (ASCII) that contains reference data (all numerics) in which
#                the columns provide time series for specific stations. The first column
#                should provide either 00000000 or a datestring YYYYMMDD:
#                Rows starting with 00000000 are considered station info (station number, lat, lon
#                etc.) and are ignored.
#                Rows starting with a datestring refer to a specific day in the time series.
#                Rows starting with "#" are completely ignored and returned unchanged
#
# ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#                Format is similar to ifile
#
# delta.file     [optional] Name of file that contains deltas (changes factors for the transformation)
#                File should contain following compulsory columns identified with compulsory headers
#                "wdf"       relative change in wet-day frequency
#                            (wet days are defined as days with 0.1 mm or more precipitation)
#                "ave"       relative change in mean precipitation
#                "p99.lower" lower   estimate of the relative change in the 99th percentile of wet-day amounts
#                "p99.centr" central estimate of the relative change in the 99th percentile of wet-day amounts
#                "p99.upper" upper   estimate of the relative change in the 99th percentile of wet-day amounts
#
#                 (If delta.file is not provided, predefined deltas are derived dependening on <sc>, <p> and <scaling>)
#
# sc             scenario                      ["GL", "GH", "WL", "WH"]
# p              time horizon                  [2030 (=DEFAULT), 2050, 2085]
# scaling        scaling extreme precipitation ["lower", "centr" (=DEFAULT), "upper"]
#
# version        "v1.1" [DEFAULT] official version that belongs to KNMI'14
#                "v1.2" alternative procedure to dry wet days
#
###########################################################################################################

#' Transformation of precipitation
#' @inheritParams temperatuur_transformatie_KNMI14
#' @param delta.file     [optional] Name of file that contains deltas (changes factors for the transformation)
#'                File should contain following compulsory columns identified with compulsory headers
#'                "wdf"       relative change in wet-day frequency
#'                            (wet days are defined as days with 0.1 mm or more precipitation)
#'                "ave"       relative change in mean precipitation
#'                "p99.lower" lower   estimate of the relative change in the 99th percentile of wet-day amounts
#'                "p99.centr" central estimate of the relative change in the 99th percentile of wet-day amounts
#'                "p99.upper" upper   estimate of the relative change in the 99th percentile of wet-day amounts
#'
#'                 (If delta.file is not provided, predefined deltas are derived dependening on <sc>, <p> and <scaling>)
#' @param scaling        scaling extreme precipitation ["lower", "centr" (=DEFAULT), "upper"]
#' @param version        "v1.1" [DEFAULT] official version that belongs to KNMI'14
#                "v1.2" alternative procedure to dry wet days
#' @export
neerslag_transformatie_KNMI14 <- function(ifile,
                                          ofile="uitvoer.txt",
                                          delta.file=NA,
                                          sc,
                                          p=2030,
                                          scaling="centr",
                                          version="v1.1") {

  # READ REFERENCE DATA FROM ifile
  H.comments <- scan(ifile, character(0), sep = "\n") # select lines with "#" from reference file and ignore them
  H.comments <- H.comments[grep("#",H.comments)]      # (only necessary for output file)

  obs        <- read.table(ifile,header=F)            # read reference data (header wordt niet apart ingelezen)
  header     <- obs[which(obs[,1]==0),]               # header met stations meta-data etc.
  header[,1] <- "00000000"
  names(obs) <- c("date",round(obs[1,-1],0))          # station names are read from first line
  obs        <- obs[which(obs[,1]!=0),]               # actual data

  # READ CHANGE FACTORS (DELTAS)
  if(!is.na(delta.file)) {
    deltas <- read.table(delta.file,header=T)         # deltas are provided in file "delta.file"
  } else {
    if(p=="2030") {
      deltas <- read.table(      "deltas-KNMI14__rr_______2030.txt",header=T) # 2030 decadal prediction if p=2030
    } else {
      # str.ext is a function used to construct file.names if delta.file is not explicitly provided
      str.ext <- function(var,ch,n) {paste(var,substr(paste(rep(ch,n),collapse=""),1,n-nchar(var)),sep="")}

      deltas <- read.table(paste("deltas-KNMI14__rr___",
                                       str.ext( sc,"_",3),"_",
                                       str.ext(  p,"_",4),".txt",sep=""),header=T)
    }
  }
  deltas$P99 <- deltas[,paste("p99",scaling,sep=".")] # choose scaling ("lower", "centr" or "upper")

  # TRANSFORMATION
  source("rr_trans_KNMI14.R")
  fut <- rr_trans_KNMI14(obs=obs, deltas=deltas, version="v1.1")


  # OUTPUT
  sink(ofile)

  # comments
  writeLines("# Transformed daily precipitation sums [mm] according to KNMI'14 transformation tool,")
  writeLines(paste("# version ",version,sep=""))
  if(is.na(p) | is.na(scaling)) {
    writeLines(paste("# Deltas are derived from ",sc,sep=""))
  } else {
    if(p=="2030") {
      writeLines("# Deltas are derived from the 2030 decadal prediction")
    } else {
      writeLines(paste("# Deltas are derived from ",sc," scenario",sep=""))
      writeLines(paste("# around the time horizon ",p,sep=""))
    }
  }
  writeLines(paste("# wet day scaling:",scaling))
  writeLines("#")
  writeLines("# Bakker A. (2015), Time series transformation tool: description of the program to")
  writeLines("# generate time series consistent with the KNMI'14 climate scenarios, TR-349")
  writeLines("#")
  for(i in 1:length(H.comments)) writeLines(H.comments[i])

  # header
# write.table(format(header[1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
# write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
# # transformed data
# write.table(format(fut,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
#JB>
  write.table(format(header[1,], width = 8),              file = "", row.names = F, col.names = F, quote = F)
  write.table(format(header[-1,], width = 8, nsmall = 3), file = "", row.names = F, col.names = F, quote = F)
  # transformed data
  write.table(format(fut, width = 8, nsmall = 2),         file = "", row.names = F, col.names = F, quote = F)
#JB<

  sink()
}












