library(profvis)
library(microbenchmark)

library(futile.logger)
flog.threshold(WARN)
# flog.threshold(DEBUG)

subscenario <- "centr"
ifile       <- "tests/testthat/regressionInput/precipitation/KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt"
ofile       <- "tmp.txt" # output file - used only temporary
delta.file  <- NA
scenario    <- "GL"
horizon     <- 2030

# ------------------------------------------------------------------------------
# Full precipitation transformation profile
# ------------------------------------------------------------------------------
profvis({
  library(knmitransformer)
  TransformPrecip(ifile=ifile, ofile=ofile, scenario=scenario, horizon=horizon,
                  subscenario=subscenario)
})

# Result: 15380 ms
# ReadInput uses ~ 27%
# rr_trans uses ~ 49% -> more or less evenly spread between
#   CalculateClimatology
#   DryWetDays
#   WetDryDays
#   TransformWetDayAmouns
# WriteOutput uses ~ 25%


# ------------------------------------------------------------------------------
# Micro benchmark of the inner precipitation transformation
# ------------------------------------------------------------------------------
input  <- knmitransformer:::ReadInput("rr", ifile)
deltas <- knmitransformer:::ReadChangeFactors(delta.file, "rr", scenario, horizon, subscenario)
# fut <- knmitransformer:::rr_trans_KNMI14(obs = input$obs, deltas = deltas, dryingScheme = "v1.1")
microbenchmark(
  knmitransformer:::rr_trans_KNMI14(   obs = input$obs, deltas = deltas, dryingScheme = "v1.1"),
  # knmitransformer:::rr_trans_KNMI14_v2(obs = input$obs, deltas = deltas, dryingScheme = "v1.1"),
  times = 10
)

# Result:
# Unit: seconds
# min      lq       mean     median   uq       max         neval
# 9.201773 9.241318 9.491148 9.293264 9.515043 10.4904     10

profvis({
  library(knmitransformer)
  knmitransformer:::rr_trans_KNMI14(obs = input$obs, deltas = deltas, dryingScheme = "v1.1")
})

# ------------------------------------------------------------------------------
# Micro benchmark for the power law exponent
# ------------------------------------------------------------------------------

th <- 0.1    # wet-day threshold

# PREPARE DATA
# explore observations
mm          <- (input$obs[,1] %/% 100) %% 100 # the month of a day (1, 2, ..., 12)
climatology <- knmitransformer:::CalculateClimatology(input$obs[, -1], deltas, mm, th)

# future values (filled with NA)
fut       <- input$obs

# TRANSFORMATION
# apply transformation per station / time series
fut[, -1] <- knmitransformer:::DryWetDays(input$obs, deltas$wdf, th, mm, dryingScheme = "v1.1")
fut[, -1] <- knmitransformer:::WetDryDays(fut[, -1], deltas$wdf, th, mm)

# fut2 <- knmitransformer:::TransformWetDayAmounts2(fut[, -1][, 1 : 20], climatology, mm, th)
# fut3 <- knmitransformer:::TransformWetDayAmounts(fut[, -1][, 1 : 20], climatology, mm, th)
# all(fut2 == fut3)

im <- 1
is <- 1
Y <- fut[, is]
wet.im <- which(im == mm & Y >= th)  # identify all wet days within calendar month <im>
Xm     <- Y[wet.im]                  # select all wet day amounts

# get climatologies for reference and future period for the month at hand
#mobs   <- climatology$mwet.obs[im,is]
qobs   <- climatology$qobs[im,is]
mfut   <- climatology$mwet.fut[im,is]
qfut   <- climatology$qfut[im,is]

microbenchmark(
  #knmitransformer:::DeterminePowerLawExponent(Xm, qfut, qobs, mfut),
  knmitransformer:::DeterminePowerLawExponentCpp(Xm, qfut, qobs, mfut)
)

microbenchmark(
  #knmitransformer:::TransformWetDayAmounts2(fut[, -1][, 1 : 20], climatology, mm, th),
  knmitransformer:::TransformWetDayAmounts(fut[, -1][, 1 : 20], climatology, mm, th),
  times = 10
)
