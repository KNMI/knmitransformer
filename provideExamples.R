# Provide temperature and precipitation example

library(data.table)
tgData <- fread("inst/exampledata/tgtg260.dat", skip = 6)

tgData[, date := format(as.Date(paste(V1, V2, V3, sep = "-"), format = "%Y-%m-%d"), format = "%Y%m%d")]

# tgData[, date2 := as.Date(paste(V1, V2, V3, sep = "-"), format = "%Y%m%d")]
#
#
# tgData[V2 == 10 & V1 == 1901, format(as.Date(paste(V1, V2, V3, sep = "-"), format = "%Y-%m-%d"), format = "%Y%m%d")]

tgData <- tgData[, .(date, tg = V4, year = V1)]


tgData <- tgData[year %in% 1981 : 2010]
obs <- tgData[, tg]
dates <- tgData[, date]
lat <- 52.1
lon <- 5.18
comment <- "# First one"


# tmp <- structure(list(data = tgData, coords = c(lat, lon), comment = comment), class = "knmiTF")
tgData[, date := as.integer(date)]

CreateValidInput <- function(data, lat, lon, comment) {
  if (ncol(data) > 2) {
    stop("data should only have a date and a tg column")
  }
  obs <- as.data.frame(data, stringsAsFactors = FALSE)
  names(obs) <- c("date", "260")
  header <- as.data.frame(cbind(rep("00000000", 5), c(rep("NA", 3), lon, lat)),
                          stringsAsFactors = FALSE)
  # Check that comments are inline with rest
  # structure(list(obs = obs, coords = c(lat, lon), comment = comment, header = header), class = "knmiTF")
  list(obs = obs, coords = c(lat, lon), comment = comment, header = header)
}

tmp <- CreateValidInput(tgData[, .(date, tg)], lat, lon, comment)

tmpTrans <- TransformTemp(tmp, NA, "GL", var = "tg", horizon = 2030, regio.file = cbind("260", "MON"))

refTrans <- TransformTemp(ref, NA, "GL", var = "tg", horizon = 2030, regio.file = "MON")
