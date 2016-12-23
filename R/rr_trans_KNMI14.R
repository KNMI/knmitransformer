################################################################################
#
# rr_trans_KNMI14.R    March 10, 2015
#
# author: Alexander Bakker (KNMI)
#
# Function 'transforms' a specific reference-dataset with daily precipitation
# sums [mm] to a dataset representative for a future climate scenario.
#
# arguments:
#
# obs            data.frame or matrix:
#                first column provides datestring YYYYMMDD
#                other columns provide precipitation [mm] time series
#                (each column represents specific station)
#
# deltas         data.frame or matrix that contains deltas (=change factors
#                for the transformation) should contain following columns
#                indicated by following headers
#                HEADER
#                "wdf"       relative change [%] in wet-day frequency
#                            (wet day is defined as day with 0.1 mm or more
#                            precipitation)
#                "ave"       relative change [%] in mean precipitation
#                "P99"       relative change [%] in the 99th percentile of
#                            wet-day amounts
#
# dryingScheme   "v1.1" [default] official version belonging to KNMI'14
#                "v1.2" alternative way to dry wet days
#
################################################################################

rr_trans_KNMI14 <- function(obs, deltas, dryingScheme = "v1.1") {

  flog.debug("Running rr_trans_KNMI14")
  version <- packageVersion("knmitransformer")
  flog.debug("Version={%s}", version)
  flog.debug("DryingScheme={%s}", dryingScheme)

  # DEFINE CONSTANTS
  th <- 0.1    # wet-day threshold

  # PREPARE DATA
  # explore observations
  mm          <- (obs[,1] %/% 100) %% 100 # the month of a day (1, 2, ..., 12)
  climatology <- CalculateClimatology(obs[, -1], deltas, mm, th)

  # future values (filled with NA)
  fut       <- obs

  # TRANSFORMATION
  # apply transformation per station / time series
  fut[, -1] <- DryWetDays(obs      , deltas$wdf, th, mm, dryingScheme)
  fut[, -1] <- WetDryDays(fut[, -1], deltas$wdf, th, mm)
  fut[, -1] <- TransformWetDayAmounts(fut[, -1], climatology, mm, th)

  return(fut)
}


DryWetDays <- function(obs, wdf, th, mm, dryingScheme) {

  # DRYING WET DAYS ##########################################################
  if(sum(wdf < 0) > 0) {   # check if reduction in wet days is needed

    nr <- length(mm)
    # add very small number (based on datestring) to ensure that all numbers in
    # time series are unique.
    # necessary for the selection of 'days to dry'
    makeUnique <- obs[, 1] * 1e-10

    for(is in 2:ncol(obs)) {
      Y <- obs[, is]

      # dryingSchme VERSION V1.1 is official KNMI14 "drying procedure" (see TR-349)
      if(dryingScheme == "v1.1") {

        # select target values
        target.values <- vector() # vector containing 'target precipitation amounts' to dry
        target.months <- vector() # vector containing the specific month to which this target values belong

        # make Y unique
        X          <- ifelse(Y < th, Y, Y + makeUnique)

        # loop all months for which a reduction of the wet day is projected
        for(im in which(wdf < 0)) {

          Xw   <- sort(X[which(X >= th & mm == im)])        # sorted vector of all wet day amounts that fall in month <im>
          ndry <- round((-1 * wdf[im] / 100) * length(Xw))  # number of days 'to dry'
          if(ndry > 0) {
            step <- length(Xw) / ndry # step size to step through wet day amount vector <Xw> (NOT AN INTEGER)

            # determine target values for month <im> (homogeneously selected from subset <Xw>)
            # and remember specific month <im> that belongs to target.values
            target.values <- c(target.values, Xw[round(((1:ndry) - 0.5) * step)])
            target.months <- c(target.months, rep(im, ndry))
          }
        }
        # assign rank order to all target.values (and belonging month-id)]
        target.months <- target.months[order(target.values)]
        target.values <- target.values[order(target.values)]

        # selection of days to dry
        droogmaken <- vector()  # vector containing the 'days to dry'
        # step through all target values from small to large
        for(idry in 1:length(target.values)) {

          # select all days that are currently available for drying
          # (during the drying procedure new wet days may become available for drying)
          available <- which(mm == target.months[idry] &  # all days within same month as target value
                             X >= th            &         # all wet days
                             (c(0, X[-nr]) < th  |        # all days preceeded and/or succeeded by dry day
                              c(X[-1], 0) < th))

          droogmaken <- c(droogmaken, available[                    # determine which of all available days is closest
            which(abs(X[available] - target.values[idry]) ==        #  to the target.value zit en put day(id) in vector
                    min(abs(X[available] - target.values[idry])))]) #  containing days to dry
          X[droogmaken[idry]] <- 0  # dry specific day in vector of adjusted values
        }
        Y[droogmaken] <- 0  # actual drying of original time series

        # END VERSION V1.1 #

      } else {

        # VERSION V1.2 has alternative procedure (not documented in TR349)
        # loop all months for which a reduction wdf is projected
        for(im in which(wdf < 0)) {

          rows    <- which(mm == im & Y >= th)                # identify all wet days in month <im>
          Xw      <- sort(Y[rows])                            # sort wet days amounts
          ndry    <- round((-1 * wdf[im] / 100) * length(Xw)) # number of wet days to dry

          if(ndry > 1) {
            c     <- Xw[ndry] # c = constant to subtract of daily values

            if(abs(length(which(Xw <= c)) - ndry) >              # is it better to lower c with respect to tied data
               abs(length(which(Xw < c)) - ndry)) {              # (i.e. days with same value) ?
              c <- ifelse(Xw[1] == c, 0, max(Xw[which(Xw < c)])) # this is important in case of small <c> and small <ndry>
            }

            # actual drying
            Y[rows] <- ifelse(Y[rows] <= c, 0, Y[rows] - c)

            # adjust empirical PDF of 'dried' data to match original empirical PDF
            n.wd    <- which(mm == im & Y >= th)    # wet days after drying (in contrast to rows)
            PP.Yw   <- rank(Y[n.wd]) / length(n.wd) # wet days empirical frequency of non-exceedance
            Y[n.wd] <- quantile(Xw, PP.Yw)          # apply distribution of Xw to frequencies of non-exceedance

          }
        }
      } # END VERSION V1.2 #
      obs[, is] <- Y
    } # END DRYING WET DAYS
  }
  return(obs[, -1])
}

WetDryDays <- function(fut, wdf, th, mm) {

  for(is in 1:ncol(fut)) {
    # WETTING DRY DAYS #######################
    Y  <- fut[, is]
    X  <- Y             # time series after drying
    nr <- length(mm)
    X1 <- c(1, X[-nr])  # precipitation of preceding day (preceding day of first day is
    # assigned 1 mm)

    for(im in 1:12) {                                     # loop through 12 calendar months

      if(wdf[im] > 0) {                              # in case an increase of wdf is projected

        rows    <- which(mm==im)                       # identify all days in month <im>
        Xm      <-  X[rows]                            #   subset all days in month <im>
        X1m     <- X1[rows]                            #      and all preceding days
        Xw      <- sort(Xm[which(Xm >= th)])           # sort all wet day values
        dwet    <- round((wdf[im] / 100) * length(Xw)) # number of 'dry days to wet'
        if(dwet > 0) {

          # select target values
          step    <- length(Xw) / dwet                      # step size to step through sorted step
          target.values <- Xw[round(((1:dwet) - 0.5) * step)] # determine target.values for month <im>
          # (homogeneously selected from sorted subset)
          # select days to wet
          preceding.wet <- cumsum(Xm >= th) + step / 2 # cumulative number of preceding wet days in month <im>
          add     <- vector()                          # vector with days that should be wetted

          for(id in 1:dwet) {
            add     <- c(add,
                         which(Xm < th &                   # select 'first' 'dry' day that succeeds a wet' day,
                               X1m >= th &                 # for which <preceding.wet> exceeds the <step> size
                               preceding.wet >= step)[1])  # and add this day(id) to vector <add>
            if(is.na(add[id])) {
              add <- add[-id]
            } else {
              preceding.wet <- preceding.wet - step        # and decrease vector <preceding.wet> with <step>
              preceding.wet[1:add[id]] <- 0
            }
          }

          # Finally, target.values are assigned to selected days
          # on the basis of the rank order of the precipitation amount of the preceding wet day
          Y[rows[add]] <- target.values[rank(X1m[add], ties.method = "first")]

        } # dfwet > 0
      } # days need to be added
    } # calander month
    fut[, is] <- Y
  }
  return(fut)
}

CalculateClimatology <- function(obs, deltas, mm, th) {

  # qq1   <- 0.99  # quantile of wet-day amounts that is used to estimate transformation coefficients
  # qq2   <- 0.90  # quantile of wet-day amounts that is used to estimate qq1 (robustly)
  # national median of monthly ratios between qq1 and qq2 for 240 precipitation stations
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

  # determine observed wet-day frequency (wdf.obs), mean (mean.obs), wet-day mean (mwet.obs), wet-day 99th percentile
  wdf.obs    <- as.matrix(aggregate(obs, by=list(mm),function(x)     mean(  x>=th      )))[,-1]
  mean.obs   <- as.matrix(aggregate(obs, by=list(mm),function(x)     mean(x            )))[,-1]
  #mwet.obs   <- as.matrix(aggregate(obs, by=list(mm),function(x)     mean(x[x>=th]     )))[,-1]
  q2.obs     <- as.matrix(aggregate(obs, by=list(mm),function(x) quantile(x[x>=th],0.90)))[,-1]
  q1.obs     <- q2.obs*ratio

  # apply deltas to observed climatology to obtain future climatology
  wdf.fut  <- wdf.obs  * (1 + deltas$wdf/100)
  mean.fut <- mean.obs * (1 + deltas$ave/100)
  mwet.fut <- mean.fut / wdf.fut
  q1.fut   <- q1.obs   * (1 + deltas$P99/100)

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


  for(is in 1:ncol(fut)) {

    Y <- fut[, is]

    for(im in 1:12) {
      wet.im <- which(im == mm & Y >= th)  # identify all wet days within calendar month <im>
      Xm     <- Y[wet.im]                  # select all wet day amounts

      # get climatologies for reference and future period for the month at hand
      #mobs   <- climatology$mwet.obs[im,is]
      qobs   <- climatology$qobs[im,is]
      mfut   <- climatology$mwet.fut[im,is]
      qfut   <- climatology$qfut[im,is]


      b <- floor(DeterminePowLawExponentCpp(Xm, qfut, qobs, mfut) * 1000) / 1000
      # b <- DeterminePowLawExponent(Xm, qfut, qobs, mfut)

      # straightforward estimation of coefficients a and c
      a  <- qfut / (qobs^b)
      c  <- a*(qobs^b) / qobs # multiplication factor for values larger than q99

      # actual transformation of wet-day amounts (application of transformation function)
      Y[wet.im] <- ifelse(Xm < qobs, a * Xm^b, c*Xm)

      Y[wet.im][which(Y[wet.im] < th)] <- th # prevent days being dried by the wet-day transformation
    }
    # END TRANSFORMATION WET-DAY AMOUNTS
    fut[, is] <- round(Y, 1)
  }
  return(fut)
}

DeterminePowLawExponent <- function(Xm, qfut, qobs, mfut) {
  # determine exponent 'b' of power-law correction function

  # function to minimise to find optimal value for coefficient 'b'
  f  <- function(b) {
    qfut / mfut -
      (qobs^b) / mean(ifelse(Xm < qobs, Xm^b, Xm * (qobs^b) / qobs))
  }

  # root finding algorithm requires that both sides of search space are
  # of opposite sign
  if(f(0.1) * f(3) < 0) {
    rc <- uniroot(f, lower = 0.1, upper = 3, tol = 0.0001)  # root finding
    return(rc$root)
  } else {
    # if root is non-existent, alternative estimation for b
    # value closest to zero is searched for
    bs <- (1:300) / 100 # determine search space for 'b'
    fs <- bs            # fs = f(bs)
    for(ifs in 1:length(fs)) {
      fs[ifs] <- f(bs[ifs])
    }
    return(bs[which(abs(fs) == min(abs(fs)))]) # b for which f(b) is smallest is chosen
  }
}

