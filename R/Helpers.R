ReadInput <- function(var, ifile) {

  types <- c("rr", "evmk", "rsds", "tg", "tn", "tx")
  if (!(var %in% types)) {
    flog.error("variable not defined.")
    stop("variable not defined.")
  }

  flog.info("Reading reference data, file={%s}", ifile)
  H.comments <- scan(ifile, character(0), sep = "\n", quiet=TRUE) # select lines with "#" from reference file and ignore them
  flog.debug("Scanning of the reference data returned n={%i} lines.", length(H.comments))
  H.comments <- H.comments[grep("#", H.comments)] # (only necessary for output file)

  obs        <- read.table(ifile, header = F)     # read reference data (header wordt niet apart ingelezen)
  header     <- obs[which(obs[, 1] == 0), ]       # header met stations meta-data etc.
  header[,1] <- paste0(rep(0, 8), collapse = "")  # "00000000"

  names(obs) <- c("date", as.integer(obs[1, -1])) # station names are read from first line
  obs        <- obs[which(obs[, 1] != 0), ]       # actual data
  flog.debug("obs colnames={%s}", paste(names(obs), collapse = ", "))

  input <- list(comments = H.comments, header = header, obs = obs)
  if (var == "rsds") {
    lat        <- as.numeric(header[5, -1])              # lat; needed for calculating radiation TOA
    # Expected as the fifth line of the header (comment added 20150601_JB)
    if (length(which(is.na(lat) == TRUE)) > 0) {
      flog.error("Check input file: Latitude NOT (or not properly) provided.")
      stop("Check input file: Latitude NOT (or not properly) provided.")
    } else {
      input$lat = lat
    }
  }
  input
}

CheckPeriod <- function(p) {
  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
    stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }
}

ReturnPackageVersion <- function(var) {
  switch(var,
         "tg"   =  flog.info("Running temperature transformation"),
         "tn"   =  flog.info("Running temperature transformation"),
         "tx"   =  flog.info("Running temperature transformation"),
         "rr"   =  flog.info("Running precipitation transformation"),
         "rsds" =  flog.info("Running radiation transformation"),
         "evmk" =  flog.info("Running evaporation transformation")
  )
  version <- paste0(packageVersion("knmitransformer"))
  flog.debug("Version={%s}", version)
  version
}

str.ext <- function(var, ch, n) {
  paste(var, substr(paste(rep(ch, n), collapse = ""), 1,
                    n - nchar(var)), sep = "")
}

ReadChangeFactors <- function(var, scenario, period, subscenario = NULL) {
  if(period == "2030") {
    if (var == "rsds") {
      tmpPath <- "deltas-KNMI14__rsds_____2030.txt"
    } else {
      tmpPath <- paste("deltas-KNMI14__", var, "_______2030.txt", sep = "")
    }
  } else {
    if (var == "rsds") {
      tmpPath <- "deltas-KNMI14__rsds_"
    } else {
      tmpPath <- paste0("deltas-KNMI14__", var, "___")
    }
    tmpPath <- paste(tmpPath,
                     str.ext(scenario, "_", 3), "_",
                     str.ext(period, "_", 4), ".txt", sep = "")
  }

  flog.info("Reading deltas, file={%s}", tmpPath)
  tmpPath <- system.file("extdata", tmpPath, package = "knmitransformer")
  deltas  <- read.table(tmpPath, header = T)

  if (var == "rr") {
    flog.info("Subscenario={%s}", subscenario)
    # choose subscenario ("lower", "centr" or "upper")
    deltas$P99 <- deltas[, paste("p99", subscenario, sep=".")]
  }
  deltas
}


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

  header <- as.data.frame(dat[1:5, ])
  header[, 1] <- paste0(rep(0, 8), collapse = "")

  write.table(format(header[1,      ], width = 8),             file = "", row.names = F, col.names = F, quote = F)
  write.table(format(header[2:5,    ], width = 8, nsmall = 3), file = "", row.names = F, col.names = F, quote = F)
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

#' Show station table
#' @keywords internal
ShowStationTable <- function() {
  tmp <- system.file("extdata", "stationstabel",
                     package = "knmitransformer")
  system(paste0("less ", tmp))
}
