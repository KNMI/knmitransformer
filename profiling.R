library(profvis)
library(microbenchmark)

library(futile.logger)
flog.threshold(WARN)
# flog.threshold(DEBUG)

scaling    <- "centr"
ifile      <- "tests/testthat/regressionInput/precipitation/KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt"
ofile      <- "tmp.txt" # output file - used only temporary
delta.file <- NA
sc="GL"

p=2030

# ------------------------------------------------------------------------------
# Full precipitation transformation profile
# ------------------------------------------------------------------------------
profvis({
  library(knmitransformer)
  neerslag_transformatie_KNMI14(ifile=ifile,
                                ofile=ofile,
                                delta.file=delta.file,
                                sc=sc,
                                p=p,
                                scaling=scaling)
})

# Result: 17250 ms
# ReadInput uses ~ 22%
# rr_trans uses ~ 57% -> 57% due to TransformWetDayAmounts -> Lion´s share by
# uniroot
# WriteOutput uses ~ 20%


# ------------------------------------------------------------------------------
# Micro benchmark of the inner precipitation transformation
# ------------------------------------------------------------------------------
input  <- knmitransformer:::ReadInput("rr", ifile)
deltas <- knmitransformer:::ReadChangeFactors(delta.file, "rr", sc, p, scaling)
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
# Further zoom on TransformWetDayAmounts
# ------------------------------------------------------------------------------
th <- 0.1
obs <- input$obs
is <- 1

ns <- ncol(obs) - 1            # number of stations (= number of columns minus 1)
mm <- (obs[,1] %/% 100) %% 100 # the month that a day belongs to (1, 2, ..., 12)
nr <- length(mm)

makeUnique <- 0.01 * obs[, 1] / 100000000

X <- obs[, is + 1]

Y <- knmitransformer:::DryWetDays(X, deltas$wdf, th, mm, makeUnique, dryingScheme = "v1.1")

Y <- knmitransformer:::WetDryDays(Y, deltas$wdf, th, mm)

tmp1 <- knmitransformer:::TransformWetDayAmounts( Y, X, deltas, mm, th)
#tmp2 <- knmitransformer:::TransformWetDayAmounts2(Y, X, deltas, mm, th)

microbenchmark(
  knmitransformer:::TransformWetDayAmounts( Y, X, deltas, mm, th),
  #knmitransformer:::TransformWetDayAmounts2(Y, X, deltas, mm, th),
  times = 100
)

profvis({
  library(knmitransformer)
  knmitransformer:::TransformWetDayAmounts( Y, X, deltas, mm, th)
  }, interval = 0.005)

profvis({
  library(knmitransformer)
  knmitransformer:::TransformWetDayAmounts2( Y, X, deltas, mm, th)
  }, interval = 0.005)

