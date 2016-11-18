#setwd("/usr/people/bakker/KNMI14/transformatie/droogte")
setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/droogte")

rm(list=ls(all=TRUE))

source("/nobackup/users/photiado/nobackup_1/bakker_home_bakup/Programma/R-functions/general_functions.R")
source("../station_transform_functions.R")

var       <- "evmk"
version   <- "v1.0"
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range     <- "19810101-20101231"

for(p in periodes) {
  for(sc in scenarios) {
    if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {
      print(paste(sc,p))
      
      rsds        <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var="rsds",range=range,v=version),sep="")) # [kJ m-2]
      tg          <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var="tg",range=range,v=version),sep="")) # [kJ m-2]
      h.ids       <- which(rsds[,1]==0)
      header      <- rsds[h.ids,]; header[,1] <- "00000000"
      names(rsds) <- c("date",round(rsds[1,-1],0))
      rsds        <- rsds[-h.ids,]
      tg          <-   tg[-h.ids,]

      ns         <- ncol(rsds) - 1
      dt         <- rsds[,1]
      nr         <- length(dt)
      
      ev         <- rsds; ev[,-1] = NA
      ev[,-1]    <- round(makkink(tg[,-1],1000*rsds[,-1]),2)
        
      # schrijf weg naar ASCII bestand
      ofile <- paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep="")
      
      sink(ofile)    
      # comments
      writeLines("# Makkink crop reference evaporarion [mm] according")
      writeLines("# transformed global radiation sums and daily mean temperature")
      writeLines("# File created from daily observations of")
      writeLines("# File created from daily observations of")
      writeLines("# Royal Netherlands Meteorological Institute (KNMI)")
      if(p != "2030") writeLines(paste("# scenario",sc))
      writeLines(paste("# time horizon",p))
      writeLines(paste("# transformation = ",version))
      # header
      write.table(format(header[1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      # transformed data
      write.table(format(ev,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
      sink()
      
    } # if(p != "2030" | sc == "G")
  }   # sc
}     # period  


