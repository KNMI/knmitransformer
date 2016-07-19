
ReadChangeFactors <- function(deltaFile, var, scenario, period) {
  if(!is.na(deltaFile)) {
    deltas <- read.table(deltaFile,header=T)         # deltas are provided in file "delta.file"
  } else {
    if(period=="2030") {
      tmpPath <- paste("deltas-KNMI14__",var,"_______2030.txt",sep="")
      tmpPath <- system.file("extdata", tmpPath, package="knmitransformer")
      deltas <- read.table(tmpPath, header=T) # 2030 decadal prediction if p=2030
    } else {
      # str.ext is a function used to construct file.names if delta.file is not explicitly provided
      str.ext <- function(var,ch,n) {paste(var,substr(paste(rep(ch,n),collapse=""),1,n-nchar(var)),sep="")}

      tmpPath <- paste("deltas-KNMI14__",var,"___",
                       str.ext(scenario, "_", 3), "_",
                       str.ext(period, "_", 4), ".txt", sep="")
      tmpPath <- system.file("extdata", tmpPath, package="knmitransformer")
      deltas <- read.table(tmpPath,header=T)
    }
  }
  deltas
}
