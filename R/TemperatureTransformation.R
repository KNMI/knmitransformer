#' tm_trans_KNMI14
#' @description Function 'transforms' a specific reference-dataset with time
#'  series of daily of mean, minimum or maximum temperature (degrees Celsius) to
#'  a dataset representative for a future climate scenario.
#' @param obs data.frame or matrix: \cr
#'  first column provides datestring YYYYMMDD \cr
#'  other columns provide precipitation (mm) time series (each column represents
#'  specific station)
#' @param deltas data.frame or matrix that contains deltas (=change factors for the transformation)
#'                should contain following columns indicated by following headers: \cr
#'               "maand"     month for which deltas are valid (1,2,...,12) \cr
#'               "P01"       1st  percentile daily temperature  \cr
#'               "P05"       5th  percentile daily temperature \cr
#'               "P50"       50th percentile daily temperature \cr
#'               "P95"       95th percentile daily temperature \cr
#'               "P99"       99th percentile daily temperature \cr
#'
#'               following column is optional in case deltas vary with region
#'               (is needed in case <regio.tabel> is provided)
#'               "regio"     region for which deltas are valid
#'                           KNMI14 distinguishes ("NWN", "ZWN", "NON", "MON", "ZON", "NLD")
#' @param regio.tabel   this (optional) argument provides a vector that relates the stations to
#'               a particular region.
#'               KNMI14 distinguishes following regions:\cr
#'               <NLD> Nederland  (DEFAULT) \cr
#'               <NWN> Noordwest Nederland \cr
#'               <ZWN> Zuidwest Nederland\cr
#'               <NON> Noordoost Nederland\cr
#'               <MON> Middenoost Nederland\cr
#'               <ZON> Zuidoost Nederland
#' @keywords internal
tm_trans_KNMI14 <- function(obs,
                            deltas,
                            regio.tabel=NA) {

  flog.debug("Running tm_trans_KNMI14")

  # PREPARE DATA
  # explore observations
  ns <- ncol(obs) - 1         # number of stations (= number of columns minus 1)
  mm <- ObtainMonth(obs[, 1]) # the month that a day belongs to (1, 2, ..., 12)

  fut <- obs
  fut[, -1] <- NA   # future values (filled with NA)

  # region information
  if (!("regio" %in% colnames(deltas))) deltas$regio <- "NLD" # add column <regio> to deltas if not provided
  deltas <- deltas[c("regio","maand","P01","P05","P50","P95","P99")] # arrange deltas

  # TRANSFORMATION
  # apply transformation per station <is> / time series
  for (is in 1:ns) {
    regio <- ifelse(is.na(regio.tabel),"NLD",regio.tabel[is]) # determine region

    for (im in 1:12) {
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

      for (ip in 3:(length(X.percentiles)-1)) {
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

      fut[days.im, is+1] <- Y
    } # END MONTHLY LOOP

  } # END OF TRANSFORMATION LOOP

  return(fut)

} # end tm_trans_KNMI14
