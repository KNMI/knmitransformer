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



