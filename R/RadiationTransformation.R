#' rsds_trans_KNMI14
#' @description Function 'transforms' a specific reference-dataset with time
#'  series of daily global radiation sums `[kJ/m2]` to a dataset representative
#'  for a future climate scenario.
#' @param obs            data.frame or matrix: \cr
#'                first column provides datestring YYYYMMDD \cr
#'                other columns provide global radiation (kJ/m2) time series
#'                (each column represents specific station)
#'
#' @param deltas         data.frame or matrix that contains deltas (=change factors for the transformation)
#'                should contain following columns indicated by following headers \cr
#'                HEADER \cr
#'                "ave"       relative change (\%) in mean shortwave surface radiation
#' @param lat vector with latitudes belonging to stations.
#'  location (row.number (i) in vector refers to column in obs (i+1)
#' @keywords internal
rsds_trans_KNMI14 <- function(obs,
                              deltas,
                              lat) {

  flog.debug("Running rsds_trans_KNMI14")

  if (ncol(obs) != length(lat) + 1) {
    err <- "Number of stations does not match length of latitude vector."
    flog.error(err)
    stop(err)
  }

  # PREPARE DATA
  # explore observations
  ns         <- ncol(obs) - 1        # number of stations (= number of columns minus 1)
  mm         <- ObtainMonth(obs[, 1]) # the month that a day belongs to (1, 2, ..., 12)
  fut        <- obs
  fut[, -1]  <- NA        # future values (filled with NA)

  # TRANSFORMATION
  # apply transformation per station / time series <is> and per calendar month <im>
  for (is in 1:ns) {
    for (im in 1:12) {

      days.im  <- which(mm == im)    # all days within in calendar month <im>
      X        <- obs[days.im, is + 1 ] # select all obs in month <im> of station <is>

      # clear sky radiation for all days in month <im>
      Rx            <- 0.75 * ObtainAngotRadiation(obs[days.im, 1], lat[is])

      # determine coefficient a for transformation function
      delta <- deltas[im, 2] / 100  # relative change of average in month <im>
      if (delta > 0) {
        a <- BoundedScaling(X, Rx, delta)
      } else {
        a <- 1 + delta
      }
      Y <- TransformRad(a, X, Rx)
      fut[days.im, is + 1] <- Y
     }
  } # END  TRANSFORMATION LOOP

  return(fut)
}


# X will not exceed Xmax
TransformRad <- function(a, X, Xmax) {
  apply(cbind(a * X, Xmax), 1, min)
}

# iterative estimation of coefficient a in function <tf>
BoundedScaling <- function(X, Xmax, delta) {
  f <- function(a) mean(TransformRad(a, X, Xmax)) - (1 + delta) * mean(X)
  rc <- uniroot(f, lower = 0, upper = 4, tol = 0.01)
  a <- rc$root
  return(a)
}

#' Obtain Top of Atmosphere radiation
#' @inheritParams ObtainDayNumber
#' @param lat latitude
#' @keywords internal
ObtainAngotRadiation <- function(date, lat) {
  gsc <- 0.0820 # solar constant [MJ/m2/min]
  phi <- pi * lat / 180
  yDay <- ObtainDayNumber(date)
  dr <- 1 + 0.033 * cos(2 * pi * yDay / 365)
  delta <- 0.409 * sin(2 * pi * yDay / 365 - 1.39)
  omega <- acos(-tan(phi) * tan(delta))
  Ra <- (24 * 60 / pi) * gsc * dr * (omega * sin(phi) * sin(delta) +
            sin(omega) * cos(phi) * cos(delta))
  1000 * Ra # unit is kJ/m2
}
