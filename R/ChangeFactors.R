str.ext <- function(var, ch, n) {
  paste(var, substr(paste(rep(ch, n), collapse = ""), 1,
                    n - nchar(var)), sep = "")
}

ReadChangeFactors <- function(deltaFile, var, scenario, period,
                              scaling = NULL) {
  if(!is.na(deltaFile)) {
    flog.info("Reading deltas, file={%s}", deltaFile)
    deltas <- read.table(deltaFile, header=T) # deltas are provided in file "delta.file"
  } else {
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
  }
  if (var == "rr") {
    flog.info("Scaling={%s}", scaling)
    # choose scaling ("lower", "centr" or "upper")
    deltas$P99 <- deltas[, paste("p99", scaling, sep=".")]
  }
  deltas
}
