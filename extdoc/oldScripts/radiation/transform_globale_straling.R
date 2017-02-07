#setwd("/usr/people/bakker/KNMI14/transformatie/globale_straling")
setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/globale_straling")

rm(list=ls(all=TRUE))

#source("/usr/people/bakker/Programma/R-functions/general_functions.R")
source("../station_transform_functions.R")

Angot <- function(datestring_YYYYMMDD,lat) {
    Gsc <- 0.0820 # solar constant [MJ/m2/min]
    phi <- pi*lat/180
    J <- daynumber(datestring_YYYYMMDD)
    dr <- 1 + 0.033*cos(2*pi*J/365)
    delta <- 0.409*sin(2*pi*J/365-1.39)
    omega <- acos(-tan(phi)*tan(delta))
    Ra <- (24*60/pi) * Gsc * dr*(omega *sin(phi)*sin(delta) + sin(omega)*cos(phi)*cos(delta))
    return(Ra)
}

 daynumber <- function(datestring_YYYYMMDD) {
    dpm <- c(0,31,59,90,120,151,181,212,243,273,304,334)
    id <- floor( datestring_YYYYMMDD %%   100)
    im <- floor((datestring_YYYYMMDD %% 10000)  / 100)
    iy <- floor( datestring_YYYYMMDD  / 10000) %%   4
    dnr <- dpm[im] + id + (iy==0 & im >2)
    return(dnr)
}

var       <- "rsds"
version   <- "v1.0"
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c(2030,2050,2085)
range="19810101-20101231"

  tf <- function(a,X,Xmax) apply(cbind(a*X,Xmax),1,min) 
bounded.scaling <- function(X,Xmax,delta) {
   f <- function(a) mean(tf(a,X,Xmax)) - (1+delta)*mean(X)
  rc <- uniroot(f, lower=0, upper=4, tol = 0.01)
   a <- rc$root
  return(a)  
}

# referentie
obs        <- read.table(paste("../tijdreeksen/",file.name(var=var,v="v1.0"),sep=""))
header     <- obs[which(obs[,1]==0),]; header[,1] <- "00000000"
names(obs) <- c("date",round(obs[1,-1],0))
obs        <- obs[which(obs[,1]!=0),]
ns         <- ncol(obs) - 1
dt         <- obs[,1]
mm         <- (obs[,1]%/%100)%%100
nr         <- length(mm)

for(p in periodes) {
  for(sc in scenarios) {
    if((p == 2030 & sc == "") | (p != 2030 & sc != "")) {
      
      deltas   <- read.table(delta.name(var,sc,p),header=T)
      fut      <- obs; fut[,-1] = NA
      
      for(is in 1:ns) {
        print(paste(is,sc,p))
        
        X   <- obs[,is+1]
        Y   <- rep(NA,length(X))
        lat <- header[5,is+1]
        
        # transform
        for(im in 1:12) {
          d.im  <- which(mm==im)                # all days within calander month im
          Xm    <- X[d.im]                      # values within calander month im
          Rx    <- 0.7*1000*Angot(dt[d.im],lat)
          delta <- deltas[im,2]/100
          if(delta > 0) {
            a <- bounded.scaling(Xm,Rx,delta)
          } else {
            a <- 1 + delta
          }
          Y[d.im] <- tf(a,Xm,Rx)
        } # im
        
        # schrijf Y weg in future tabel
        fut[,is+1] <- round(Y,1)
      } # station is
      
      # schrijf weg naar ASCII bestand
      ofile <- paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep="")
      
      sink(ofile)    
      # comments
      writeLines("# Transformed global radiation sums [kJ/m2]")
      writeLines("# according to KNMI'14 climate change scenarios")
      writeLines("# File created from daily observations of")
      writeLines("# Royal Netherlands Meteorological Institute (KNMI)")
      if(p != "2030") writeLines(paste("# scenario",sc))
      writeLines(paste("# time horizon",p))
      writeLines(paste("# transformation = ",version))
      # header
      write.table(format(header[1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      # transformed data
      write.table(format(fut,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
      sink()
      
    } # if(p != "2030" | sc == "G")
  }   # sc
}     # period


