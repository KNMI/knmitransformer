#' rr_trans_KNMI14.R
#' @description Function 'transforms' a specific reference-dataset with daily
#'  precipitation sums (mm) to a dataset representative for a future climate
#'  scenario.
#' @param obs data.frame or matrix: \cr
#'                first column provides datestring YYYYMMDD \cr
#'                other columns provide precipitation (mm) time series
#'                (each column represents specific station)
#' @param deltas  data.frame or matrix that contains deltas (=change factors
#'                for the transformation) should contain following columns
#'                indicated by following headers
#'                HEADER\cr
#'                "wdf"       relative change (\%) in wet-day frequency
#'                            (wet day is defined as day with 0.1 mm or more
#'                            precipitation) \cr
#'                "ave"       relative change (\%) in mean precipitation \cr
#'                "P99"       relative change (\%) in the 99th percentile of
#'                            wet-day amounts
#' @keywords internal
rr_trans_KNMI14 <- function(obs, deltas) {

  flog.debug("Running rr_trans_KNMI14")

  # DEFINE CONSTANTS
  th <- 0.1    # wet-day threshold

  # PREPARE DATA
  # explore observations
  mm          <- ObtainMonth(obs[, 1]) # the month of a day (1, 2, ..., 12)
  climatology <- CalculateClimatology(obs[, -1, drop = FALSE], deltas, mm, th)

  # future values (filled with NA)
  fut       <- obs

  # TRANSFORMATION
  # apply transformation per station / time series
  fut[, -1] <- DryWetDays(obs,                     deltas$wdf, th, mm)
  fut[, -1] <- WetDryDays(fut[, -1, drop = FALSE], deltas$wdf, th, mm)
  fut[, -1] <- TransformWetDayAmounts(fut[, -1, drop = FALSE], climatology, mm, th)

  return(fut)
}


DryWetDays <- function(obs, wdf, th, mm) {

  # DRYING WET DAYS ##########################################################
  if (sum(wdf < 0) > 0) {
    # check if reduction in wet days is needed

    flog.debug("Drying wet days")

    # add very small number (based on datestring) to ensure that all numbers in
    # time series are unique.
    # necessary for the selection of 'days to dry'
    makeUnique <- obs[, 1] * 1e-10

    for (is in 2:ncol(obs)) {
      Y <- obs[, is]

      # select target values
      target.values <- vector()
      target.months <- vector()

      # make Y unique
      X          <- ifelse(Y < th, Y, Y + makeUnique)

      # loop all months for which a reduction of the wet day is projected
      for (im in which(wdf < 0)) {

        # sorted vector of all wet day amounts that fall in month <im>
        Xw   <- sort(X[which(X >= th & mm == im)])
        # number of days 'to dry'
        ndry <- round(-wdf[im] / 100 * length(Xw))
        if (ndry > 0) {
          # step size to step through wet day amount vector <Xw> (NOT AN INTEGER)
          step <- length(Xw) / ndry

          # determine target values for month <im> (homogeneously selected from <Xw>)
          # and remember specific month <im> that belongs to target.values
          target.values <- c(target.values, Xw[round(((1:ndry) - 0.5) * step)]) #nolint
          target.months <- c(target.months, rep(im, ndry))
        }
      }
      # assign rank order to all target.values (and belonging month-id)]
      target.months <- target.months[order(target.values)]
      target.values <- target.values[order(target.values)]

      # selection of days to dry
      toDry <- SelectDaysToDry(mm, target.values, target.months, X, th)
      Y[toDry] <- 0  # actual drying of original time series

      obs[, is] <- Y
    } # END DRYING WET DAYS
  }
  return(obs[, -1])
}

SelectDaysToDry <- function(mm, target.values, target.months, X, th) {
  nr <- length(mm)
  # selection of days to dry
  droogmaken <- vector()  # vector containing the 'days to dry'
  # step through all target values from small to large
  for (idry in 1:length(target.values)) {

    # select all days that are currently available for drying
    # (during the drying procedure new wet days may become available for drying)
    # daysInTargetMonth <-
    available <- which(mm == target.months[idry] &  # all days within same motarget value #nolint
                         X >= th            &       # all wet days
                         (c(0, X[-nr]) < th  |      # all days preceeded and/or succeeded by dry day #nolint
                            c(X[-1], 0) < th))

    # determine which of all available days is closest
    # to the target.value zit en put day(id) in vector
    # containing days to dry
    droogmaken <- c(droogmaken, available[
      which(abs(X[available] - target.values[idry]) ==
              min(abs(X[available] - target.values[idry])))])
    X[droogmaken[idry]] <- 0  # dry specific day in vector of adjusted values
  }
  droogmaken
}

WetDryDays <- function(fut, wdf, th, mm) {

  flog.debug("Wetting dry days")

  for (is in 1:ncol(fut)) {
    # WETTING DRY DAYS #######################
    Y  <- fut[, is]
    X  <- Y             # time series after drying
    nr <- length(mm)
    X1 <- c(1, X[-nr])  # precipitation of preceding day (preceding day of first day is
    # assigned 1 mm)

    for (im in 1:12) {
      # loop through 12 calendar months

      if (wdf[im] > 0) {
        # in case an increase of wdf is projected

        rows    <- which(mm == im)                     # identify all days in month <im>
        Xm      <- X[rows]                             #   subset all days in month <im>
        X1m     <- X1[rows]                            #      and all preceding days
        Xw      <- sort(Xm[which(Xm >= th)])           # sort all wet day values
        dwet    <- round((wdf[im] / 100) * length(Xw)) # number of 'dry days to wet' #nolint
        if (dwet > 0) {

          # select target values
          # step size to step through sorted step
          step    <- length(Xw) / dwet
          # determine target.values for month <im>
          target.values <- Xw[round( ( (1:dwet) - 0.5) * step)]
          # (homogeneously selected from sorted subset)
          # select days to wet
          # cumulative number of preceding wet days in month <im>
          preceding.wet <- cumsum(Xm >= th) + step / 2
          add     <- vector()   # vector with days that should be wetted

          for (id in 1:dwet) {
            # select 'first' 'dry' day that succeeds a wet' day,
            # for which <preceding.wet> exceeds the <step> size
            # and add this day(id) to vector <add>
            add     <- c(add,
                         which(Xm < th &
                               X1m >= th &
                               preceding.wet >= step)[1])
            if (is.na(add[id])) {
              add <- add[-id]
            } else {
              # and decrease vector <preceding.wet> with <step>
              preceding.wet <- preceding.wet - step
              preceding.wet[1:add[id]] <- 0
            }
          }

          # Finally, target.values are assigned to selected days
          # on the basis of the rank order of the precipitation amount of the
          # preceding wet day
          Y[rows[add]] <- target.values[rank(X1m[add], ties.method = "first")]

        }
      }
    }
    fut[, is] <- Y
  }
  return(fut)
}

CalculateClimatology <- function(obs, deltas, mm, th) {

  flog.debug("Calculate climatology")

  # national median of monthly ratios between wet-day 0.99-quantile and
  # 0.90-quantile for 240 precipitation stations (to make qq1 more robust)
  ratio <- c(2.22,
             2.271,
             2.366,
             2.147,
             2.346,
             2.166,
             2.276,
             2.404,
             2.476,
             2.087,
             2.336,
             2.18)

  # determine observed:
  # wet-day frequency (wdf.obs),
  # mean (mean.obs),
  # # wet-day mean (mwet.obs),
  # wet-day 99th percentile (q1.obs)
  wdf.obs    <- as.matrix(aggregate(obs, by = list(mm),
      function(x)     mean( x >= th        )))[, -1, drop = FALSE]
  mean.obs   <- as.matrix(aggregate(obs, by = list(mm),
      function(x)     mean(x               )))[, -1, drop = FALSE]
  q2.obs     <- as.matrix(aggregate(obs, by = list(mm),
      function(x) quantile(x[x >= th], 0.90)))[, -1, drop = FALSE]
  q1.obs     <- q2.obs * ratio

  # apply deltas to observed climatology to obtain future climatology
  wdf.fut  <- wdf.obs  * (1 + deltas$wdf / 100)
  mean.fut <- mean.obs * (1 + deltas$ave / 100)
  mwet.fut <- mean.fut / wdf.fut
  q1.fut   <- q1.obs   * (1 + deltas$P99 / 100)

  list(#mwet.obs = RemoveDimNames(mwet.obs),
       mwet.fut = RemoveDimNames(mwet.fut),
       qobs     = RemoveDimNames(q1.obs),
       qfut     = RemoveDimNames(q1.fut))
}

RemoveDimNames <- function(x) {
  dims <- dim(x)
  x <- as.numeric(x)
  dim(x) <- dims
  x
}

TransformWetDayAmounts <- function(fut, climatology, mm, th) {

  flog.debug("Transform wet day amounts")

  for (is in 1:ncol(fut)) {

    Y <- fut[, is]

    for (im in 1:12) {
      # identify all wet days within calendar month <im>
      wet.im <- which(im == mm & Y >= th)
      Xm     <- Y[wet.im]                  # select all wet day amounts

      # get climatologies for reference and future period for the month at hand
      qobs   <- climatology$qobs[im, is]
      mfut   <- climatology$mwet.fut[im, is]
      qfut   <- climatology$qfut[im, is]

      b <- floor(DeterminePowerLawExponentCpp(Xm, qfut, qobs, mfut) * 1000) / 1000

      # straightforward estimation of coefficients a and c
      a  <- qfut / (qobs^b) #nolint
      c  <- a * qobs^b / qobs # factor for values larger than q99 # nolint

      # actual transformation of wet-day amounts (application of transformation function)
      Y[wet.im] <- ifelse(Xm < qobs, a * Xm^b, c * Xm) #nolint

      # prevent days being dried by the wet-day transformation
      Y[wet.im][which(Y[wet.im] < th)] <- th
    }
    # END TRANSFORMATION WET-DAY AMOUNTS
    fut[, is] <- Y
  }
  return(fut)
}

# DeterminePowerLawExponent <- function(Xm, qfut, qobs, mfut) {
#   # determine exponent 'b' of power-law correction function
#
#   # function to minimise to find optimal value for coefficient 'b'
#   f  <- function(b) {
#     qfut / mfut -
#       (qobs^b) / mean(ifelse(Xm < qobs, Xm^b, Xm * (qobs^b) / qobs))
#   }
#
#   # root finding algorithm requires that both sides of search space are
#   # of opposite sign
#   if(f(0.1) * f(3) < 0) {
#     rc <- uniroot(f, lower = 0.1, upper = 3, tol = 0.00001)  # root finding
#     return(rc$root)
#   } else {
#     # if root is non-existent, alternative estimation for b
#     # value closest to zero is searched for
#     bs <- (1:300) / 100 # determine search space for 'b'
#     fs <- bs            # fs = f(bs)
#     for(ifs in 1:length(fs)) {
#       fs[ifs] <- f(bs[ifs])
#     }
#     return(bs[which(abs(fs) == min(abs(fs)))]) # b for which f(b) is smallest is chosen
#   }
# }
