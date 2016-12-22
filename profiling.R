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

# Result:
# ReadInput uses ~ 16%
# rr_trans uses ~ 68% -> 74% due to TransformWetDayAmounts
# WriteOutput uses ~ 15%


###
input <- knmitransformer:::ReadInput("rr", ifile)
deltas <- knmitransformer:::ReadChangeFactors(delta.file, "rr", sc, p, scaling)
# fut <- knmitransformer:::rr_trans_KNMI14(obs = input$obs, deltas = deltas, dryingScheme = "v1.1")
microbenchmark(
  knmitransformer:::rr_trans_KNMI14(obs = input$obs, deltas = deltas, dryingScheme = "v1.1"),
  times = 10
)

# Result:
# Unit: seconds
# min      lq       mean     median   uq       max         neval
# 15.49553 15.59089 15.69025 15.66138 15.74696 16.04444    10


