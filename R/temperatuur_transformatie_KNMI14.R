###########################################################################################################
#
# temperatuur_transformatie_KNMI14.R    March 11, 2015
#
# author: Alexander Bakker (KNMI)
#
# Function reads 'reference data' with ime series of daily of mean, minimum or maximum temperature [degrees
# Celsius] and 'change factors' from input files and applies them to function 'tm_trans_KNMI14' to obtain
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
#               - HEADER -
#               "maand"     month for which deltas are valid (1,2,...,12)
#               "P01"       1st  percentile daily temperature
#               "P05"       5th  percentile daily temperature
#               "P50"       50th percentile daily temperature
#               "P95"       95th percentile daily temperature
#               "P99"       99th percentile daily temperature
#
#               following column is optional in case deltas vary with region
#               (is needed in case <regio.tabel> is provided)
#               "regio"     region for which deltas are valid
#                           KNMI14 distinguishes ("NWN", "ZWN", "NON", "MON", "ZON", "NLD")
#
#                 (If delta.file is not provided, predefined deltas are derived dependening on <sc>, <p> and
#                  daily temperature characteristic of interest [mean, min or max])
#
# sc             scenario                      ["GL", "GH", "WL", "WH"]
# p              time horizon                  [2030 (=DEFAULT), 2050, 2085]
# var            kind of daily temperature variable ["tg" = daily mean,
#                                                    "tn" = daily minimum,
#                                                    "tx" = daily maximum]
#
# regio.file     this (optional) argument provides the name of an ASCII file that relates the stations to
#                a particular region. First column is station id and second column region
#                KNMI14 distinguishes following regions:
#                <NLD> Nederland            [DEFAULT]
#                <NWN> Noordwest Nederland
#                <ZWN> Zuidwest Nederland
#                <NON> Noordoost Nederland
#                <MON> Middenoost Nederland
#                <ZON> Zuidoost Nederland
#
###########################################################################################################

#' Transformation of temperature
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
#' @export
temperatuur_transformatie_KNMI14 <- function(ifile,
                                              ofile="uitvoer.txt",
                                              delta.file=NA,
                                              sc,
                                              p=2030,
                                              var,
                                              regio.file=NA) {

  # CONSTANTS AND FUNCTIONS ###############################################################################
  version="v1.0"

  # READ REFERENCE DATA FROM ifile
  H.comments <- scan(ifile, character(0), sep = "\n") # select lines with "#" from reference file and ignore them
  H.comments <- H.comments[grep("#",H.comments)]      # (only necessary for output file)

  obs        <- read.table(ifile,header=F)            # read reference data (header wordt niet apart ingelezen)
  header     <- obs[which(obs[,1]==0),]               # header met stations meta-data etc.
  header[,1] <- "00000000"
  names(obs) <- c("date",round(obs[1,-1],0))          # station names are read from first line
  obs        <- obs[which(obs[,1]!=0),]               # actual data

 print(names(obs))
#print(obs[1:10,])

  # READ CHANGE FACTORS (DELTAS)
  if(!is.na(delta.file)) {
    deltas <- read.table(delta.file,header=T)         # deltas are provided in file "delta.file"
  } else {
    if(p=="2030") {
      tmpPath <- paste("deltas-KNMI14__",var,"_______2030.txt",sep="")
      tmpPath <- system.file("extdata", tmpPath,
                             package="knmitransformer")
      deltas <- read.table(tmpPath, header=T) # 2030 decadal prediction if p=2030
    } else {
      # str.ext is a function used to construct file.names if delta.file is not explicitly provided
      str.ext <- function(var,ch,n) {paste(var,substr(paste(rep(ch,n),collapse=""),1,n-nchar(var)),sep="")}

      deltas <- read.table(paste("deltas-KNMI14__",var,"___",
                                 str.ext( sc,"_",3),"_",
                                 str.ext(  p,"_",4),".txt",sep=""),header=T)
    }
  }

#print(deltas)

  # LINK STATIONS TO REGIONS
  if(is.na(regio.file)) {
    regio.tabel <- NA
  } else {
    tmpPath <- system.file("extdata", "stationstabel", package="knmitransformer")
    stationstabel <- read.table(tmpPath)
    regio.tabel   <- as.vector(stationstabel[match(names(obs)[-1],stationstabel[,1]),2])
  }

 print(regio.tabel)

  # TRANSFORMATION
  #source("tm_trans_KNMI14.R")
  fut <- tm_trans_KNMI14(obs=obs, deltas=deltas, regio.tabel=regio.tabel)

  # OUTPUT #############################################################################################
  sink(ofile)

  # comments
  writeLines("# Transformed daily temperature [deg.C] according to KNMI'14 transformation tool,")
  writeLines(paste("# version ",version,sep=""))
  if(is.na(p)) {
    writeLines(paste("# Deltas are derived from ", sc, sep=""))
  } else {
    if(p=="2030") {
      writeLines("# Deltas are derived from the 2030 decadal prediction")
    } else {
      writeLines(paste("# Deltas are derived from ", sc ," scenario", sep=""))
      writeLines(paste("# around the time horizon ", p, sep=""))
    }
  }
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


  } # end function temp.trans_KNMI14
