#' rsds_trans_KNMI14
#' @description Function 'transforms' a specific reference-dataset with time
#'  series of daily global radiation sums [kJ/m2] to a dataset representative
#'  for a future climate scenario.
#' @param obs            data.frame or matrix: \cr
#'                first column provides datestring YYYYMMDD \cr
#'                other columns provide global radiation [kJ/m2] time series
#'                (each column represents specific station)
#'
#' @param deltas         data.frame or matrix that contains deltas (=change factors for the transformation)
#'                should contain following columns indicated by following headers \cr
#'                HEADER \cr
#'                "ave"       relative change [\%] in mean shortwave surface radiation
#' @param lat vector with latitudes belonging to stations.
#'  location (row.number [i]) in vector refers to column in obs [i+1]
rsds_trans_KNMI14 <- function(obs,
                              deltas,
                              lat) {

  flog.debug("Running rsds_trans_KNMI14")

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

  # TRANSFORMATION
  # apply transformation per station / time series <is> and per calendar month <im>
  for(is in 1:ns) {
    for(im in 1:12) {

      days.im       <- which(mm == im)       # all days within in calendar month <im>
      X             <- obs[days.im,is+1]     # select all obs in month <im> of station <is>

      # clear sky radiation for all days in month <im>
      # version v1.0 applied accidently factor 0.7 (rather than 0.75)
      Rx            <- 0.7 * Angot(obs[days.im,1], lat[is])

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


# "Angot" or "Top Of Atmoshphere" radiation
Angot <- function(datestring_YYYYMMDD,lat) {
  Gsc <- 0.0820 # solar constant [MJ/m2/min]
  phi <- pi*lat/180
  J <- daynumber(datestring_YYYYMMDD)
  dr <- 1 + 0.033*cos(2*pi*J/365)
  delta <- 0.409*sin(2*pi*J/365-1.39)
  omega <- acos(-tan(phi)*tan(delta))
  Ra <- (24*60/pi) * Gsc * dr*(omega *sin(phi)*sin(delta) + sin(omega)*cos(phi)*cos(delta))
  1000*Ra # unit is kJ/m2
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
