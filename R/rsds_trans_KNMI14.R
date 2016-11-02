###########################################################################################################
#
# rsds_trans_KNMI14.R    March 11, 2015
#
# author: Alexander Bakker (KNMI)
#
# Function 'transforms' a specific reference-dataset with time series of daily global radiation sums [kJ/m2]
# to a dataset representative for a future climate scenario.
#
# Transformation developed for KNMI'14 climate change scenarios for the Netherlands:
#
# Bakker, A. (2015), Time series transformation tool: description of the program to generate time series
# consistent with the KNMI’14 climate scenarios, Technical Report TR-348, De Bilt, the Netherlands
#
# Version History:
#   v1.0 - October,  2014
#   v1.1 - March 11, 2015 [adjustment factor to estimate clear sky radiation]
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
#                other columns provide global radiation [kJ/m2] time series
#                (each column represents specific station)
#
# deltas         data.frame or matrix that contains deltas (=change factors for the transformation)
#                should contain following columns indicated by following headers
#                HEADER
#                "ave"       relative change [%] in mean shortwave surface radiation
#
# lat            this argument provides a vector with latitudes belonging to stations.
#                location (row.number [i]) in vector refers to column in obs [i+1]
#
###########################################################################################################

rsds_trans_KNMI14 <- function(obs,
                              deltas,
                              lat) {

  flog.debug("Running rsds_trans_KNMI14")
  version="v1.1"

  # PREPARE DATA
  # explore observations
  ns         <- ncol(obs) - 1                      # number of stations (= number of columns minus 1)
  mm         <- (obs[,1]%/%100)%%100               # the month that a day belongs to (1, 2, ..., 12)
  nr         <- length(mm)                         # total number of days (in reference file)

  fut      <- obs; fut[,-1] = NA                   # future values (filled with NA)

  # FUNCTIONS USED FOR TRANSFORMATION

  # transformation function
  # X will not exceed Xmax
  tf <- function(a,X,Xmax) apply(cbind(a*X,Xmax),1,min)

  # iterative estimation of coefficient a in function <tf>
  bounded.scaling <- function(X,Xmax,delta) {
    f <- function(a) mean(tf(a,X,Xmax)) - (1+delta)*mean(X)
    rc <- uniroot(f, lower=0, upper=4, tol = 0.01)
    a <- rc$root
    return(a)
  }

  # "Angot" or "Top Of Atmoshphere" radiation
  Angot <- function(datestring_YYYYMMDD,lat) {
    Gsc <- 0.0820 # solar constant [MJ/m2/min]
    phi <- pi*lat/180
    J <- daynumber(datestring_YYYYMMDD)
    dr <- 1 + 0.033*cos(2*pi*J/365)
    delta <- 0.409*sin(2*pi*J/365-1.39)
    omega <- acos(-tan(phi)*tan(delta))
    Ra <- (24*60/pi) * Gsc * dr*(omega *sin(phi)*sin(delta) + sin(omega)*cos(phi)*cos(delta))
    return(Ra)
  }

  # Derive daynumber within year (1-366)
  daynumber <- function(datestring_YYYYMMDD) {
    dpm <- c(0,31,59,90,120,151,181,212,243,273,304,334)
    id <- floor( datestring_YYYYMMDD %%   100)
    im <- floor((datestring_YYYYMMDD %% 10000)  / 100)
    iy <- floor( datestring_YYYYMMDD  / 10000) %%   4
    dnr <- dpm[im] + id + (iy==0 & im >2)
    return(dnr)
  }


  # TRANSFORMATION
  # apply transformation per station / time series <is> and per calendar month <im>
  for(is in 1:ns) {
    for(im in 1:12) {

      days.im       <- which(mm == im)       # all days within in calendar month <im>
      X             <- obs[days.im,is+1]     # select all obs in month <im> of station <is>

      # clear sky radiation for all days in month <im>
      # version v1.0 applied accidently factor 0.7 (rather than 0.75)
      Rx            <- 0.7 * 1000 * Angot(obs[days.im,1], lat[is])

      # determine coefficient a for transformation function
      delta <- deltas[im,2]/100                # relative change of average in month <im>
      if(delta > 0) {
        a <- bounded.scaling(X, Rx, delta)
      } else {
        a <- 1 + delta
      }
      Y <- tf(a, X, Rx)                      # transform
      fut[days.im,is+1] <- round(Y,1)
      # fut[days.im,is+1] <- round(Y,2)        # round results and write to fut  #20150629_JB
     }
  } # END  TRANSFORMATION LOOP

  return(fut)

} # end function rsds_trans_KNMI14


