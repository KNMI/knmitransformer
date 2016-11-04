WriteOutput <- function(var, ofile, version, sc, p, H.comments, header, fut) {
  flog.info("Write output")
  sink(ofile)

  # comments
  writeLines("# Transformed daily mean global radiation [kJ/m2] according to KNMI'14 transformation tool,")
  writeLines(paste("# version ",version,sep=""))
  if(is.na(p)) {
    writeLines(paste("# Deltas are derived from ",sc,sep=""))
  } else {
    if(p=="2030") {
      writeLines("# Deltas are derived from the 2030 decadal prediction")
    } else {
      writeLines(paste("# Deltas are derived from ",sc," scenario",sep=""))
      writeLines(paste("# around the time horizon ",p,sep=""))
    }
  }
  writeLines("#")
  writeLines("# Bakker A. (2015), Time series transformation tool: description of the program to")
  writeLines("# generate time series consistent with the KNMI'14 climate scenarios, TR-349")
  writeLines("#")
  for(i in 1:length(H.comments)) writeLines(H.comments[i])

  # header
  # write.table(format(header[1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
  # write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
  # # transformed data
  # write.table(format(fut,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
  #JB>
  write.table(format(header[1,], width = 8),              file = "", row.names = F, col.names = F, quote = F)
  write.table(format(header[-1,], width = 8, nsmall = 3), file = "", row.names = F, col.names = F, quote = F)
  # transformed data
  write.table(format(fut, width = 8, nsmall = 2),         file = "", row.names = F, col.names = F, quote = F)
  #JB<

  sink()

  fread(ofile)
}