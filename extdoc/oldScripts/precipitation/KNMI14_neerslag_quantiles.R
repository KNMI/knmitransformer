setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")
rm(list=ls(all=TRUE))

var="rr"

#versions  <- c("v0.1","v0.2")
versions  <- c("v1.1")
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range="19810101-20101231"

precquantiles.name <- function(sc2p,transversion) {return(paste("../kwantielen/KNMI14",sc2p,transversion,"neerslagkwantielen.txt",sep="_"))}

frequencies   <- c(1:99/100,1:9/1000+0.99)
fquantiles    <- function(x) quantile(x,frequencies)

get.quantiles <- function(sc=sc,p=p,scaling=scaling,version=version) {
  
  print(c(sc,p,scaling,paste("version:",version)))
  
  # read data
  ifile <- ifelse(p!="ref",
                  paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version,scaling=scaling),sep=""),
                  paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v="v1.0",scaling=""),sep=""))
  
  rr         <- read.table(ifile)
  header     <- rr[which(rr[,1]==0),]; header[,1] <- "00000000"
  names(rr)  <- c("date",round(rr[1,-1],0))
  station    <- which(!is.na(match(names(rr),"550"))) # N_550 is De Bilt
  rr         <- rr[which(rr[,1]!=0),c(1,station)]
  
  mm <- (rr[,1]%/%100)%%100
  ss <- as.integer((mm/3)%%4+1)
  
  tabel <- as.data.frame(matrix(NA,4*(length(frequencies)),ncol(rr)))
  names(tabel) <- names(rr)
  
  # seasonal variables
  i=0
  for(season in 1:4) {
    id  <- which(ss==season)
    
    #if(length(frequencies)>0) {
    #  for(j in 1:length(drempels)) {
    i=i+1
    rows  <- ((season-1)*length(frequencies))+1:length(frequencies)
    dname <- paste("qq",frequencies,season,sep="_")
    tabel[rows, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
    tabel[rows,-1] <- ifelse(ncol(rr)>2,apply(rr[id,-1],2,fquantiles),fquantiles(rr[id,-1]))
  } # end seasonal variables
  
  
}
  
for(version in versions) {
  for(p in periodes) {
    for(sc in scenarios) {
      if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {
        for(scaling in c("lower","upper","centr")) {
          
          
          
          
          
          
          
          
#      yy <-  rr[,1]%/%10000
#      wy <- ifelse(mm<12,yy,yy+1)
      
      
      # schrijf weg naar ASCII bestand
      ofile <- ifelse(p=="reference",precquantiles.name(sc2p,refversion),precquantiles.name(sc2p,transversion))
      
      sink(ofile)    
      # comments
      writeLines("# Climatologies derived from transformed daily precipitation observations")
      writeLines("# Royal Netherlands Meteorological Institute (KNMI)")
      writeLines("# processed for the KNMI-14 climate change scenarios")
      writeLines(paste("# version = ",refversion))
      #writeLines(paste("# documentation = Release notes Thema6 WP3 datasets"))
      if(p == "MOC" | p == "EOC") writeLines(paste("# scenario = ",sc))
      writeLines(paste("# period = ",p))
      writeLines(paste("# transformation = ",transversion))
      # header
      write.table(format(header[ 1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      # transformed data
      write.table(format(tabel,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
      sink()
    } # if(p == "MOC" | p == "EOC" | sc == "G") {
  }   # sc
}     # period
  


