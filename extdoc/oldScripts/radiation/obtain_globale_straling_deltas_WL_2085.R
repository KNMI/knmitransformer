#20150519 Adapted by Jules Beersma 
#20150519 to include factor 7/6 also for WL_2085 (correction of error made until now)
#20150519 corrected error since factor 7/6 was also not applied to WH_2085

#setwd("/usr/people/bakker/KNMI14/transformatie/globale_straling")
setwd("/nobackup_1/users/beersma/bakker_home_backup/KNMI14/transformatie/globale_straling")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

maanden   <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
vars      <- read.table("../variabelen.txt",quote="\"",header=T)
stats=c("ave")



##directory <- "/usr/people/lenderin/KNXT12/AnalysisFromMark/scenarios_2030_MOC_EOC_set7XL/"

##file.resamp.NLD <- function(directory,var,perc,sc,p,timestep="DD") {
##  return(paste(directory,"NLD.",var,".",perc,".",timestep,".ERE_resamp_",sc,".",p,".monthly.dat",sep=""))
##}

##file.noresamp.NLD <- function(directory,var,perc,timestep="DD") {
##  return(paste(directory,"NLD.",var,".",perc,".",timestep,".ERE_noresamp.2016-2045.monthly.dat",sep=""))
##}

variabelen = c("rsds")
for(v in match(variabelen,vars[,3])) {
  var  =as.character(vars[v,3]); 
  var.G=as.character(vars[v,2])
  
  # scenarios
# for(p in c(2050,2085)) {
  for(p in c(2085)) {
    p.G <- c("MOC","EOC")[match(p,c(2050,2085))]
#   for(sc in c("GL","GH","WL","WH")) {
    for(sc in c("WL", "WH")) {
      sc.G <- c("G","G_plus","W","W_plus")[match(sc,c("GL","GH","WL","WH"))]
      
      # get delta's Geert
      # let op: opmaak NLD; ALL en precip.scaling tabel zij verschillend
      tabel        <- data.frame(matrix(NA,12,(1+length(stats))))
      names(tabel) <- c("maand",stats)
      tabel[,1]    <- 1:12 #maandnummers
      for(i in 1:length(stats)) {
        tabel[stats[i]] <- read.table(file.resamp("NLD",var.G,stats[i],sc.G,p.G,timestep="MM"),skip=1)[,5] # read MM ipv DD #20150519JB waarom?
      }
      # scaling factor 7/6 for W_plus EOC (= WH_2085)
#     if (p == 2085 & sc == "WH") {tabel[-1:-2] <- round(tabel[-1:-2] * 7 / 6,3)}  #20150519JB Dit is dus fout, factor 7/6 wordt NIET toegepast!!!
      if (p == 2085 & sc == "WH") {tabel[-1] <- round(tabel[-1] * 7/6, 3)}
      # scaling factor 7/6 for W EOC (= WL_2085)
      if (p == 2085 & sc == "WL") {tabel[-1] <- round(tabel[-1] * 7/6, 3)}
      
      write.table(tabel,delta.name(var,sc,p),quote=F,row.names=F)
    } # sc
  } # p
  
  # decadale voorspelling
# NOT NEEDED #20150519JB
#  tabel        <- data.frame(matrix(NA,12,(1+length(stats))))
#  names(tabel) <- c("maand",stats)
#  tabel[,1]    <- 1:12 #maandnummers
#  for(i in 1:length(stats)) {
#    tabel[stats[i]] <- read.table(file.noresamp("NLD",var.G,stats[i],timestep="MM"),skip=1)[,5] # read MM ipv DD
#  }
#  write.table(tabel,delta.name(var,sc="",p=2030),quote=F,row.names=F)
  
} # v in vars

