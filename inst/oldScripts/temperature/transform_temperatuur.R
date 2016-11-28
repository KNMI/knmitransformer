#'CP: change 
#setwd("/usr/people/bakker/KNMI14/transformatie/temperatuur")

setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/temperatuur")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

##CP: change the version for the output
version   <- "v1.0_T25" #c("v1.0")
version_out   <- "v2.0_T25"
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c(2030,2050,2085)
range="19810101-20101231"

# STRAKS (MISSCHIEN) AANPASSEN #
stationstabel <- read.table("../regions/stationscoordinaten.txt")[,c(1,10)]
################################

# temperatuur
for(var in c("tg","tn","tx")) {
  # referentie
  obs        <- read.table(paste("../tijdreeksen/",file.name(var=var,v=version),sep=""))
  header     <- obs[which(obs[,1]==0),]; header[,1] <- "00000000"
  names(obs) <- c("date",round(obs[1,-1],0))
  obs        <- obs[which(obs[,1]!=0),]
  ns         <- ncol(obs) - 1
  mm         <- (obs[,1]%/%100)%%100
  nr         <- length(mm)
  
  for(p in periodes) {
    for(sc in scenarios) {
      
      if((p == 2030 & sc == "") | (p != 2030 & sc != "")) {

        print(paste(var,sc,p))
        
        fut    <- obs; fut[,-1] = NA
        deltas <- read.table(delta.name(var,sc,p),header=T)
        
        for(is in (1:ns)+1) {
          for(im in 1:12) {
            station <- as.numeric(names(obs)[is])
            regio   <- as.character(stationstabel[which(stationstabel[,1]==station),2])
            dperc   <- as.numeric(deltas[which(deltas[,1]==regio & deltas[,2]==im),-1:-2])
            
            ids     <- which(mm == im)                             # rows with data in calendar month im
            X       <- obs[ids,is]                                 # observations from month im with station is
            Y       <- rep(NA,length(X))
            Xp      <- as.numeric(quantile(X,c(1,5,50,95,99)/100,na.rm=T)) # observed quantiles
            Yp      <- Xp + dperc                                  # target   quantiles of transformed time series
            
            # linear transformation: for intervals X<qq5, qq5<X<qq50, qq50<X<qq95, qq95<X
            ip=2 # X < Xp[2]
            xids      <- which(X<Xp[ip])
            a         <- (Yp[ip]-Yp[ip-1])/(Xp[ip]-Xp[ip-1])
            b         <-  Yp[ip] - a*Xp[ip]
            Y[xids]   <- a*X[xids] + b
            
            for(ip in 3:(length(Xp)-1)) {
              xids    <- which(X>=Xp[ip-1] & X<Xp[ip])
              a       <- (Yp[ip]-Yp[ip-1])/(Xp[ip]-Xp[ip-1])
              b       <-  Yp[ip] - a*Xp[ip]
              Y[xids] <- a*X[xids] + b
            }
                        
            ip=length(Xp)
            xids      <- which(X>=Xp[ip-1]) # let op
            a         <- (Yp[ip]-Yp[ip-1])/(Xp[ip]-Xp[ip-1])
            b         <-  Yp[ip] - a*Xp[ip]
            Y[xids]   <- a*X[xids] + b
            
            # schrijf Y weg in future tabel
            fut[ids,is] <- round(Y,1)
          }
        }

      # schrijf weg naar ASCII bestand
      ofile <- paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep="")
      
      sink(ofile)    
      # comments
      stat <-c("mean","min","max")[match(var,c("tg","tn","tx"))]  
      writeLines(paste("#",stat,"temperature [degrees Celsius]"))  
      writeLines("# Transformed daily observations of Royal Netherlands Meteorological Institute (KNMI)")
      writeLines("# (data that have been homogenised prior to transformation are marked by 1, Brandsma et al., 2014)")
      writeLines("# transformation according to KNMI'14 climate change scenarios")
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
}       # variable
  


