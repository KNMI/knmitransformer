WriteOutput <- function(var, ofile, version, sc, p, H.comments, dat,
                        subscenario = NULL) {
  flog.info("Write output")
  sink(ofile)

  # comments
  if (var == "rsds") {
    writeLines("# Transformed daily mean global radiation [kJ/m2] according to KNMI'14 transformation tool,")
  } else if (var %in% c("tg", "tn", "tx")) {
    writeLines("# Transformed daily temperature [deg.C] according to KNMI'14 transformation tool,")
  } else if (var == "rr") {
    writeLines("# Transformed daily precipitation sums [mm] according to KNMI'14 transformation tool,")
  } else if (var == "evmk") {
    writeLines("# Transformed daily Makkink evaporation [mm] according to KNMI'14 transformation tool,")
  }

  writeLines(paste("# version ", version, sep=""))
  if(p=="2030") {
    writeLines("# Deltas are derived from the 2030 decadal prediction")
  } else {
    writeLines(paste("# Deltas are derived from ", sc ," scenario", sep=""))
    writeLines(paste("# around the time horizon ", p, sep=""))
  }
  writeLines("#")
  writeLines("# Bakker A. (2015), Time series transformation tool: description of the program to")
  writeLines("# generate time series consistent with the KNMI'14 climate scenarios, TR-349")
  writeLines("#")
  if (var == "rr") {
    writeLines(paste("# wet day subscenario:", subscenario))
    writeLines("#")
  }
  for(i in 1:length(H.comments)) writeLines(H.comments[i])

  write.table(format(dat[1,      ], width = 8),             file = "", row.names = F, col.names = F, quote = F)
  write.table(format(dat[2:5,    ], width = 8, nsmall = 3), file = "", row.names = F, col.names = F, quote = F)
  write.table(format(dat[-(1:5), ], width = 8, nsmall = 2), file = "", row.names = F, col.names = F, quote = F)

  sink()
}



