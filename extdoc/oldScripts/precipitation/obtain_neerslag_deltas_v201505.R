#20150519 Adapted by Jules Beersma 
#20150519 to include new ranges of P99w provided by Geert Lenderink on 20150513, and 
#20150519 to include factor 7/6 also for WL_2085 (correction of error made until now)
#20150519 NB functions file.resamp.scaling and file.noresamp.scaling are introduced since new "scaling".dat files of Geert are in the directory: 
#            /usr/people/lenderin/KNXT12/AnalysisFromMark/precipextremes/                 rather than 
#            /usr/people/lenderin/KNXT12/AnalysisFromMark/scenarios_2030_MOC_EOC_set7XL

#setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")
setwd("/nobackup_1/users/beersma/bakker_home_backup/KNMI14/transformatie/neerslag")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")
#20150519>
# "scaling".dat bronbestanden van Geert
file.resamp.scaling   <- function(area="ALL", var, stat, sc, p,             timestep="DD") {
  dir = "/usr/people/lenderin/KNXT12/AnalysisFromMark/precipextremes/"
  return(paste(dir, area, ".", var, ".", stat, ".", timestep, ".ERE_resamp_", sc, ".", p, ".monthly.dat", sep=""))
}
file.noresamp.scaling <- function(area="ALL", var, stat,     p="2016-2045", timestep="DD") {
  dir = "/usr/people/lenderin/KNXT12/AnalysisFromMark/precipextremes/"
  return(paste(dir, area, ".", var, ".", stat, ".", timestep, ".ERE_noresamp."       , p, ".monthly.dat", sep=""))
}
#20150519<


maanden   <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
vars      <- read.table("../variabelen.txt",quote="\"",header=T)
stats     <- c("wdf","ave","P50w","P90w") # P99w apart afleiden
scaling   <- "P99w.scaling"

# neerslag
var   = "rr"
var.G = as.character(vars[match(var,vars[,3]),2])

# scenarios
# scenarios
for(p in c(2050,2085)) {
  p.G <- c("MOC","EOC")[match(p,c(2050,2085))]
  for(sc in c("GL","GH","WL","WH")) {
    sc.G <- c("G","G_plus","W","W_plus")[match(sc,c("GL","GH","WL","WH"))]
    
    # get delta's Geert
    # let op: opmaak NLD; ALL en precip.scaling tabel zij verschillend
    tabel        <- data.frame(matrix(NA,12,(1+length(stats)+3)))
    names(tabel) <- c("maand",stats,"p99.lower","p99.centr","p99.upper")
    tabel[,1]    <- 1:12 #maandnummers
    for(i in 1:length(stats)) {
      tabel[stats[i]] <- read.table(file.resamp("NLD",var.G,stats[i],sc.G,p.G),skip=1)[,5]
    }
#   tabel[c("p99.lower","p99.upper")] <- read.table(file.resamp("NLD",var.G,scaling,sc.G,p.G),skip=1)[,5:6]
    tabel[c("p99.lower","p99.upper")] <- read.table(file.resamp.scaling("NLD",var.G,scaling,sc.G,p.G),skip=1)[,5:6]  #20150519JB
    tabel["p99.centr"]                <- (tabel["p99.lower"] + tabel["p99.upper"]) / 2
      
    # scaling factor 7/6 for W_plus EOC (= WH_2085)
    # let op: opmaak tabel[-1] ipv tabel[-1:-2] want geen regionale verschillen
#   if(p == 2085 & sc == "WH") {tabel[-1] <- round(tabel[-1] * 7 / 6,3)}
    if(p == 2085 & sc == "WH") {tabel[-1] <- round(tabel[-1] * 7/6, 3)}
#20150519>
    # scaling factor 7/6 for W EOC (= WL_2085)
    if(p == 2085 & sc == "WL") {tabel[-1] <- round(tabel[-1] * 7/6, 3)}
#20150519<
      
    write.table(tabel,delta.name(var,sc,p),quote=F,row.names=F)
  }
}


  # decadale voorspelling [#20150519JB dwz 2030]
tabel        <- data.frame(matrix(NA,12,(1+length(stats)+3)))
names(tabel) <- c("maand",stats,"p99.lower","p99.centr","p99.upper")
tabel[,1]    <- maanden
for(i in 1:length(stats)) {
  tabel[stats[i]] <- read.table(file.noresamp("NLD",var.G,stats[i]),skip=1)[,5]
}
#tabel[c("p99.lower","p99.upper")] <- read.table(file.noresamp("NLD",var.G,scaling),skip=1)[,5:6]
 tabel[c("p99.lower","p99.upper")] <- read.table(file.noresamp.scaling("NLD",var.G,scaling),skip=1)[,5:6] #20150519JB
 tabel["p99.centr"]                <- (tabel["p99.lower"] + tabel["p99.upper"]) / 2

write.table(tabel,delta.name(var,sc="",p=2030),quote=F,row.names=F)
