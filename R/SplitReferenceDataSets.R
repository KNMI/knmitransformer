SplitReferenceFile <- function(filename) {
  comments <- scan(filename, character(0), sep = "\n", quiet=TRUE) # select lines with "#" from reference file and ignore them
  comments <- comments[grep("#", comments)]      # (only necessary for output file)

  reference     <- fread(filename, data.table = FALSE)

  for( i in 2 : ncol(reference)) {
    tmp <- reference[, c(1,i)]

    header     <- tmp[which(tmp[,1]==0),]               # header met stations meta-data etc.
    header[,1] <- "00000000"
    obs        <- tmp[which(tmp[,1]!=0),]

    ofile <- paste0(substr(filename, 1, nchar(filename)-4),
                    "_", header[1, 2], ".txt")

    sink(ofile)

    # comments
    writeLines("# Splitted reference data file")
    writeLines(paste0("# '", filename, "'"))
    writeLines("# into individual files")
    writeLines("#")
    for(i in 1:length(comments)) writeLines(comments[i])
    write.table(format(header[ 1, ], width = 8),             file = "", row.names = F, col.names = F, quote = F)
    write.table(format(header[-1, ], width = 8, nsmall = 3), file = "", row.names = F, col.names = F, quote = F)
    write.table(format(obs, width = 8, nsmall = 2),          file = "", row.names = F, col.names = F, quote = F)

    sink()
  }
}
