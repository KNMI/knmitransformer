setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

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
    tabel[c("p99.lower","p99.upper")] <- read.table(file.resamp("NLD",var.G,scaling,sc.G,p.G),skip=1)[,5:6]
    tabel["p99.centr"]                <- (tabel["p99.lower"] + tabel["p99.upper"]) / 2
      
    # scaling factor 7/6 for W_plus EOC
    # let op: opmaak tabel[-1] ipv tabel[-1:-2] want geen regionale verschillen
    if(p == 2085 & sc == "WH") {tabel[-1] <- round(tabel[-1] * 7 / 6,3)}
      
    write.table(tabel,delta.name(var,sc,p),quote=F,row.names=F)
  }
}
  # decadale voorspelling
tabel        <- data.frame(matrix(NA,12,(1+length(stats)+3)))
names(tabel) <- c("maand",stats,"p99.lower","p99.centr","p99.upper")
tabel[,1]    <- maanden
for(i in 1:length(stats)) {
  tabel[stats[i]] <- read.table(file.noresamp("NLD",var.G,stats[i]),skip=1)[,5]
}
tabel[c("p99.lower","p99.upper")] <- read.table(file.noresamp("NLD",var.G,scaling),skip=1)[,5:6]
tabel["p99.centr"]                <- (tabel["p99.lower"] + tabel["p99.upper"]) / 2

write.table(tabel,delta.name(var,sc="",p=2030),quote=F,row.names=F)
