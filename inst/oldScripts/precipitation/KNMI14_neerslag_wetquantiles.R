setwd("/usr/people/bakker/KvK-Thema6/KNMI14-transformatie/neerslag")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")
refversion="0001"
transversion="0001" #"0002"
rtimespan="19810101T080000_20110101T080000"

#products <- data.frame("sd.Psum"=1)

#drempels <- c(0.1,0.3,1.0,5.0,10.0,20.0)
th=0.1

frequencies   <- c(1:99/100,1:9/1000+0.99)
fquantiles    <- function(x) quantile(x,frequencies)

precquantiles.name <- function(sc2p,transversion) {return(paste("../klimatologieen/KNMI14",sc2p,transversion,"natte_kwantielen.txt",sep="_"))}


for(p in c("reference","2030","EOC","MOC")) {
  for(sc in c("G","G_plus","W","W_plus")) {
      
    if(p == "MOC" | p == "EOC" | sc == "G") {
      if(p=="2030" | p=="reference") {
        sc_p <- p
        sc2p <- p
      } else {
        sc_p <- paste(sc,p,sep="_")
        sc2p <- cat.sc.p(sc,p)
      }
      print(sc_p)
      
      if(p == "reference") {
        rr        <- read.table(ref.name("rr_tap",rtimespan,refversion))
      } else {  
        rr        <- read.table(trans.name("rr",rtimespan,transversion,sc2p))
      }
      header    <- rr[which(rr[,1]==0),]; header[,1] <- "00000000"
      names(rr) <- c("date",round(rr[1,-1],0))
      rr        <- rr[-1:-5,]
            
      mm <- (rr[,1]%/%100)%%100
      ss <- as.integer((mm/3)%%4+1)
      
      tabel <- as.data.frame(matrix(NA,4*(length(frequencies)),ncol(rr)))
      names(tabel) <- names(rr)
      
      # seasonal variables
      for(season in 1:4) {
        id    <- which(ss==season)
        rows  <- ((season-1)*length(frequencies))+1:length(frequencies)
        dname <- paste("qq",frequencies,season,sep="_")
        tabel[rows, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        
        for(is in 2:ncol(rr)) {
          tabel[rows,is] <- fquantiles(rr[id,is][which(rr[id,is]>=0.1)])
        }
      } # end seasonal variables
      
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
  


