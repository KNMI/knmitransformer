library(profvis)
library(microbenchmark)

library(futile.logger)
flog.threshold(WARN)
# flog.threshold(DEBUG)

subscenario <- "centr"
input       <- system.file("refdata",
                           "KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt",
                           package="knmitransformer")
ofile       <- "tmp.txt" # output file - used only temporary
scenario    <- "GL"
horizon     <- 2050

# ------------------------------------------------------------------------------
# Full precipitation transformation profile
# ------------------------------------------------------------------------------
profvis({
  library(knmitransformer)
  TransformPrecip(input=input, ofile=ofile, scenario=scenario, horizon=horizon,
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
input  <- knmitransformer:::ReadInput("rr", input)
deltas <- knmitransformer:::ReadChangeFactors("rr", scenario, horizon, subscenario)
# fut <- knmitransformer:::rr_trans_KNMI14(obs = input$obs, deltas = deltas)
microbenchmark(
  knmitransformer:::rr_trans_KNMI14(   obs = input$obs, deltas = deltas),
  # knmitransformer:::rr_trans_KNMI14_v2(obs = input$obs, deltas = deltas),
  times = 10
)

# Result:
# Unit: seconds
# min      lq       mean     median   uq       max         neval
# 9.201773 9.241318 9.491148 9.293264 9.515043 10.4904     10

profvis({
  library(knmitransformer)
  knmitransformer:::rr_trans_KNMI14(obs = input$obs, deltas = deltas)
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
fut[, -1] <- knmitransformer:::DryWetDays(input$obs, deltas$wdf, th, mm)
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

# ------------------------------------------------------------------------------
# Full temperature transformation profile
# ------------------------------------------------------------------------------

var        <- "tg"
input      <- system.file("refdata",
                          "KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                          package="knmitransformer")
regio.file <- "stationstabel"
scenario   <- "GL"
horizon    <- 2050
ofile       <- "tmp.txt" # output file - used only temporary

profvis({
  library(knmitransformer)
  TransformTemp(input=input, ofile=ofile, scenario=scenario, horizon=horizon,
                var=var, regio.file = regio.file)
})

# ------------------------------------------------------------------------------
# Full radiation transformation profile
# ------------------------------------------------------------------------------
input    <- system.file("refdata",
                        "KNMI14____ref_rsds___19810101-20101231_v3.2.txt",
                        package="knmitransformer")
scenario <- "GL"
horizon  <- 2030
ofile    <- "tmp.txt" # output file - used only temporary

profvis({
  library(knmitransformer)
  TransformRadiation(input=input, ofile=ofile,
                     scenario=scenario, horizon=horizon)
})

# ------------------------------------------------------------------------------
# Full evaporation transformation profile
# ------------------------------------------------------------------------------

input_tg   <- system.file("refdata",
                          "KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                          package="knmitransformer")
input_rsds <- system.file("refdata",
                          "KNMI14____ref_rsds___19810101-20101231_v3.2.txt",
                          package="knmitransformer")
scenario    <- "GL"
horizon     <- 2050
regio.file  <- "stationstabel"
ofile       <- "tmp.txt" # output file - used only temporary

profvis({
  library(knmitransformer)
  TransformEvap(input_tg=input_tg, input_rsds=input_rsds, ofile=ofile,
                scenario=scenario, horizon=horizon,
                regio.file="stationstabel")
})
