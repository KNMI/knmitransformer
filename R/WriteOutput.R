WriteOutput <- function(var, ofile, version, sc, p, H.comments, dat,
                        subscenario = NULL) {
  flog.info("Write output")
  sink(ofile)

  # comments
  if (var == "rsds") {
    writeLines("# Transformed daily mean global radiation [kJ/m2]")
  } else if (var %in% c("tg", "tn", "tx")) {
    writeLines("# Transformed daily temperature [deg.C]")
  } else if (var == "rr") {
    writeLines("# Transformed daily precipitation sums [mm]")
  } else if (var == "evmk") {
    writeLines("# Transformed daily Makkink evaporation [mm]")
  }
  writeLines("# according to KNMI'14 climate change scenarios.")
  if (var == "rr") {
    writeLines("# File created from daily (homogenised) observations of")
  } else {
    writeLines("# File created from daily observations of")
  }
  writeLines("# Royal Netherlands Meteorological Institute (KNMI).")

  writeLines("#")
  writeLines(paste0("# Time horizon: ", p))
  if (p != 2030) {
    writeLines(paste0("# Scenario: ", sc))
  }
  if (var == "rr") {
    writeLines(paste0("# Subscenario: ", subscenario))
  }
  writeLines(paste0("# Version ", version))
  writeLines("#")

  #for(i in 1:length(H.comments)) writeLines(H.comments[i])

  write.table(format(dat[1,      ], width = 8),             file = "", row.names = F, col.names = F, quote = F)
  write.table(format(dat[2:5,    ], width = 8, nsmall = 3), file = "", row.names = F, col.names = F, quote = F)
  # write.table(format(dat[-(1:5), ], width = 8, nsmall = 2), file = "", row.names = F, col.names = F, quote = F)
  date <- dat[-(1:5), 1]
  tmp  <- dat[-(1:5), -1]
  if (var == "rsds") {
    write.table(format(cbind(date, round(tmp)), width = 8, nsmall = 0), file = "", row.names = F, col.names = F, quote = F)
  } else if (var == "evmk") {
    write.table(format(cbind(date, round(tmp, 2)), width = 8, nsmall = 2), file = "", row.names = F, col.names = F, quote = F)
  } else {
    write.table(format(cbind(date, round(tmp, 1)), width = 8, nsmall = 1), file = "", row.names = F, col.names = F, quote = F)
  }

  sink()
}



