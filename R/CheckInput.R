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
