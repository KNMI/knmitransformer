###########################################################################################################
#
# tm_trans_KNMI14.R    March 11, 2015
#
# author: Alexander Bakker (KNMI)
#
# Function 'transforms' a specific reference-dataset with time series of daily of mean, minimum or maximum
# temperature [degrees Celsius] to a dataset representative for a future climate scenario.
#
# Transformation developed for KNMI'14 climate change scenarios for the Netherlands:
#
# Bakker, A. (2015), Time series transformation tool: description of the program to generate time series
# consistent with the KNMI’14 climate scenarios, Technical Report TR-348, De Bilt, the Netherlands
# 
# Version History:
#   v1.0 - March 11, 2015 (initial version)
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
# obs            data.frame or matrix:
#                first column provides datestring YYYYMMDD
#                other columns provide precipitation [mm] time series (each column represents specific station)
#
# deltas         data.frame or matrix that contains deltas (=change factors for the transformation)
#                should contain following columns indicated by following headers
#                HEADER
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
# regio.tabel   this (optional) argument provides a vector that relates the stations to 
#               a particular region. Vector provides references to region. Location (rownumber [i]
#               in vector refers to column in obs [i+1]
#
#               KNMI14 distinguishes following regions:
#               <NLD> Nederland            [DEFAULT]
#               <NWN> Noordwest Nederland
#               <ZWN> Zuidwest Nederland
#               <NON> Noordoost Nederland
#               <MON> Middenoost Nederland
#               <ZON> Zuidoost Nederland
#
###########################################################################################################

tm_trans_KNMI14 <- function(obs,
                            deltas,
                            regio.tabel=NA) {
  
  version="v1.0"
  
  # PREPARE DATA
  # explore observations
  ns         <- ncol(obs) - 1                      # number of stations (= number of columns minus 1)
  mm         <- (obs[,1]%/%100)%%100               # the month that a day belongs to (1, 2, ..., 12)
  nr         <- length(mm)                         # total number of days (in reference file)
  
  fut      <- obs; fut[,-1] = NA                   # future values (filled with NA)
  
  # region information
  if(!("regio" %in% colnames(deltas))) deltas$regio <- "NLD" # add column <regio> to deltas if not provided
  deltas <- deltas[c("regio","maand","P01","P05","P50","P95","P99")] # arrange deltas
  
  # TRANSFORMATION
  # apply transformation per station <is> / time series
  for(is in 1:ns) {
    regio <- ifelse(is.na(regio.tabel),"NLD",regio.tabel[is]) # determine region
    
    for(im in 1:12) {
      # get change factors for specific station and month
      percentile.changes <- as.numeric(
        deltas[which(deltas[,1]==regio & deltas[,2]==im),-1:-2] )
      
      days.im       <- which(mm == im)                                     # all days within in calendar month <im>
      X             <- obs[days.im,is+1]                                   # select all obs in month <im> of station <is>
      Y             <- rep(NA,length(X))
      X.percentiles <- as.numeric(quantile(X,c(1,5,50,95,99)/100,na.rm=T)) # observed percentiles
      Y.percentiles <- X.percentiles + percentile.changes                  # estimation of future percentiles
      
      # linear transformation: for intervals X<qq5, qq5<X<qq50, qq50<X<qq95, qq95<X
      # ip = percentile id
      
      ip=2      # X < X.percentile[2] (5th percentile)
      x.ids     <- which(X < X.percentiles[ip])                  # id's for all values smaller than second smallest
      # estimate linear function for percentile range
      a         <- (Y.percentiles[ip] - Y.percentiles[ip-1]) /
                   (X.percentiles[ip] - X.percentiles[ip-1])
      b         <-  Y.percentiles[ip] - a * X.percentiles[ip]
      Y[x.ids]   <- a * X[x.ids] + b                             # apply function to all temperatures below 5th percentile
      
      for(ip in 3:(length(X.percentiles)-1)) {
        x.ids    <- which(X >= X.percentiles[ip-1] & X < X.percentiles[ip]) # id's all values within analysed percentile range
        a        <- (Y.percentiles[ip] - Y.percentiles[ip-1]) /
                    (X.percentiles[ip] - X.percentiles[ip-1])
        b        <-  Y.percentiles[ip] - a * X.percentiles[ip]
        Y[x.ids]  <- a * X[x.ids] + b
      }
      
      ip=length(X.percentiles)
      x.ids      <- which(X >= X.percentiles[ip-1])                         # id's for all values larger than second largest
      a          <- (Y.percentiles[ip] - Y.percentiles[ip-1]) /
                   (X.percentiles[ip] - X.percentiles[ip-1])
      b          <-  Y.percentiles[ip] - a * X.percentiles[ip]
      Y[x.ids]   <- a * X[x.ids] + b

#     fut[days.im, is+1] <- round(Y,1)                           # round results and write to fut
      fut[days.im, is+1] <- round(Y,2)                           # round results and write to fut  #20150626_JB
    } # END MONTHLY LOOP
    
  } # END OF TRANSFORMATION LOOP
  
  return(fut)

} # end tm_trans_KNMI14
