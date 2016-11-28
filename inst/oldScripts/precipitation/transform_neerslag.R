#'CP: change path
#setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")

setwd("/nobackup/users/photiado/nobackup_1/KNMI14_scenarios/transformatie/neerslag")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

versions  <- c("v1.1","v1.2")
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c(2030,2050,2085)
range="19810101-20101231"

# drempel
th=0.1
qq1=0.99
qq2=0.90

var="rr"

# referentie
obs        <- read.table(paste("../tijdreeksen/",file.name(var=var,v="v1.0"),sep=""))
header     <- obs[which(obs[,1]==0),]; header[,1] <- "00000000"
names(obs) <- c("date",round(obs[1,-1],0))
obs        <- obs[which(obs[,1]!=0),]
ns         <- ncol(obs) - 1
mm         <- (obs[,1]%/%100)%%100
nr         <- length(mm)
  
ratio      <- read.table("ratio_Q99_Q90.txt")[,2]
  
wdf.obs    <- as.matrix(aggregate(obs[,-1],by=list(mm),function(x) mean(  x>=th)))[,-1]
mean.obs   <- as.matrix(aggregate(obs[,-1],by=list(mm),function(x) mean(x)))[,-1]
mwet.obs   <- as.matrix(aggregate(obs[,-1],by=list(mm),function(x) mean(x[x>=th])))[,-1]
q2.obs     <- as.matrix(aggregate(obs[,-1],by=list(mm),function(x) quantile(x[x>=th],0.90)))[,-1]
q1.obs     <- q2.obs*ratio
  
for(version in versions) {
  for(p in periodes) {
    for(sc in scenarios) {
      if((p == 2030 & sc == "") | (p != 2030 & sc != "")) {
        for(scaling in c("lower","upper","centr")) {
          
          # target values
          deltas     <- read.table(delta.name(var,sc,p),header=T)
          deltas$P99 <- deltas[,paste("p99",scaling,sep=".")]
          
          fut      <- obs; fut[,-1] = NA
          wdf.fut  <- wdf.obs  * (1 + deltas$wdf/100)
          mean.fut <- mean.obs * (1 + deltas$ave/100)
          mwet.fut <- mean.fut / wdf.fut
          q1.fut   <- q1.obs  * (1 + deltas$P99/100)
          
          for(is in 1:ns) {
            print(c(is,sc,p,scaling,paste("version:",version)))
            
            Y  <- obs[,is+1]
            
            # drying wet days
            if(sum(deltas$wdf<0) > 0) {
              #if(version=="v0.1") {                          # oude droogmaakprocedure
              if(!is.na(match(version,c("v0.1","v1.1")))) {   # oude droogmaakprocedure
                # select target values/days
                tvalues <- vector()
                tmonths <- vector()
                X  <- ifelse(Y<th,Y,Y + 0.01*obs[,1]/100000000) # create unique values only
                for(im in which(deltas$wdf<0)) {
                  Xw   <- sort(X[which(X>=th & mm==im)])
                  ndry <- round((-1*deltas$wdf[im]/100)*length(Xw))
                  if(ndry > 0) {
                    stap <- length(Xw)/ndry
                    tvalues <- c(tvalues,Xw[round(((1:ndry)-0.5)*stap)])
                    tmonths <- c(tmonths,rep(im,ndry))
                  }
                }
                tmonths <- tmonths[order(tvalues)]
                tvalues <- tvalues[order(tvalues)]
                
                # actual drying
                droogmaken <- vector()
                for(idry in 1:length(tvalues)) {                             # select days for drying
                  #tvalues[idry]; tmonths[idry]
                  available <- which(mm          ==tmonths[idry] &
                                       X          >=th            &
                                       (c(0, X[-nr])     <th            |
                                          c(   X[-1] ,0)   <th)           )
                  
                  droogmaken <- c(droogmaken,available[
                    which(abs(X[available]-tvalues[idry]) == 
                            min(abs(X[available]-tvalues[idry])))])
                  X[droogmaken[idry]] <- 0 
                } # for (idry = 1:ndry): actual drying
                Y[droogmaken] <- 0
                
              } else { # version = "v0.2" or "v1.2"
                for(im in which(deltas$wdf<0)) {
                  rows    <- which(mm==im & Y>=th)                     # days in month <im> and more than <th> prec 
                  Xw      <- sort(Y[rows])                             # sort wet days
                  ndry    <- round((-1*deltas$wdf[im]/100)*length(Xw)) # number of wet days to dry
                  if(ndry > 1) {
                    c       <- Xw[ndry]                                # c = constant to subtract of daily values
                    if(abs(length(which(Xw<=c)) - ndry) >              # is it better to lower c with respect to tied data?
                         abs(length(which(Xw< c)) - ndry)) {
                      c <- ifelse(Xw[1]==c,0,max(Xw[which(Xw<c)]))
                    }
                    
                    # actual drying
                    Y[rows] <- ifelse(Y[rows]<=c,0,Y[rows]-c)
                    n.wd    <- which(mm==im & Y>=th)                   # wet days after drying (in contrast to rows)
                    PP.Yw   <- rank(Y[n.wd])/length(n.wd)              # wet days sample frequency of non-exceedance
                    Y[n.wd] <- quantile(Xw,PP.Yw)                      # apply distribution of Xw to frequencies of non-exceedance
                    
                  } # im
                } # version 2
                
              } # which transform version?
            } # drying wet days
            
            # wetting dry days
            X  <- Y
            X1 <- c(1,X[-nr])
            for(im in 1:12) {
              if(deltas$wdf[im]>0) {
                rows    <- which(mm==im)
                Xm      <-  X[rows]
                X1m     <- X1[rows]
                Xw      <- sort(Xm[which(Xm>=th)])
                dwet    <- round((deltas$wdf[im]/100)*length(Xw))
                if(dwet > 0) {
                  # select target values
                  stap    <- length(Xw)/dwet
                  tvalues <- Xw[round(((1:dwet)-0.5)*stap)]
                  # select days to wet
                  precwet <- cumsum(Xm>=0.1) + stap/2 # cum. preceding wet days in mm=im
                  add     <- vector()
                  for(id in 1:dwet) {
                    add     <- c(add,which(Xm<th & X1m>=0.1 & precwet>=stap)[1])
                    if(is.na(add[id])) {
                      add <- add[-id]
                    } else {
                      precwet <- precwet - stap
                      precwet[1:add[id]] <- 0
                    }
                  }
                  Y[rows[add]] <- tvalues[rank(X1m[add],ties.method="first")]
                } # dfwet > 0
              } # days need to be added
            } # calander month
            
            # transforming wet days
            #determine coefficients
            for(im in 1:12) {
              wet.im <- which(im==mm & Y>=th)
              Xm   <- Y[wet.im]
              mobs <- as.numeric(mwet.obs[im,is])
              qobs <- as.numeric(q1.obs[im,is])
              mfut <- as.numeric(mwet.fut[im,is])
              qfut <- as.numeric(q1.fut[im,is])
              
              # bepaal machtscoefficient b
              #f  <- function(b) qfut/mfut - (qobs^b)/mean(Xm^b)
              f  <- function(b) qfut/mfut - (qobs^b)/mean(ifelse(Xm<qobs,Xm^b,Xm*(qobs^b)/qobs))
              if(f(0.1)*f(3)<0) {
                rc <- uniroot(f,lower=0.1,upper=3,tol=0.001)
                b  <- rc$root
              } else {
                bs <- (1:300)/100
                fs <- bs
                for(ifs in 1:length(fs)) {
                  fs[ifs] <- f(bs[ifs])
                }
                b <- bs[which(abs(fs)==min(abs(fs)))]
              }
              a  <- qfut/(qobs^b)
              c  <- a*(qobs^b)/qobs # factor voor waarden groter dan q99
              
              # transformeer
              # Y[wet.im] <- a*Xm^b # verkeerd                # v0.1 en v0.2
              Y[wet.im] <- ifelse(Xm<qobs,a*Xm^b,c*Xm)        # v1.1 en v1.2
              
              Y[wet.im][which(Y[wet.im]<th)] <- th # prevent days being dried by the transformation
            } # im
            
            # schrijf Y weg in future tabel
            fut[,is+1] <- round(Y,1)
          } # station is
          
          # schrijf weg naar ASCII bestand
          ofile <- paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version,scaling=scaling),sep="")
          
          sink(ofile)    
          # comments
          #stat <-c("mean","min","max")[match(var,c("tg","tn","tx"))]  
          #writeLines(paste("#",stat,"temperature [degrees Celsius]"))
          writeLines("# Transformed daily precipitation sums")
          writeLines("# according to KNMI'14 climate change scenarios")
          writeLines("# File created from daily (homogenised) observations of")
          writeLines("# Royal Netherlands Meteorological Institute (KNMI)")
          if(p != "2030") writeLines(paste("# scenario",sc))
          writeLines(paste("# time horizon",p))
          writeLines(paste("# wet day scaling",scaling))
          writeLines(paste("# transformation = ",version))
          
          # header
          write.table(format(header[1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
          write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
          # transformed data
          write.table(format(fut,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
          sink()
          
        } # scaling
      }   # if(p != "2030" | sc == "G")
    }     # scenarios
  }       # periods
}         # versions

  


