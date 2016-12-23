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

