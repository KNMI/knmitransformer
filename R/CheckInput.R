CheckPeriod <- function(p) {
  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
    stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }
}

ReturnPackageVersion <- function() {
  flog.info("Running temperature transformation")
  version <- paste0(packageVersion("knmitransformer"))
  flog.debug("Version={%s}", version)
  version
}
