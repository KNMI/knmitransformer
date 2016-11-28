setwd("/usr/people/bakker/KNMI14/transformatie/temperatuur")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

maanden  <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
vars     <- read.table("../variabelen.txt",quote="\"",header=T)
stats    <- c("P01","P05","P50","P95","P99")

# temperatuur
variabelen = c("tg","tn","tx")
for(v in match(variabelen,vars[,3])) {
  var=as.character(vars[v,3]); var.G=as.character(vars[v,2])
  
  # scenarios
  for(p in c(2050,2085)) {
    p.G <- c("MOC","EOC")[match(p,c(2050,2085))]
    for(sc in c("GL","GH","WL","WH")) {
      sc.G <- c("G","G_plus","W","W_plus")[match(sc,c("GL","GH","WL","WH"))]
      
      # get delta's Geert
      tabel        <- data.frame(matrix(NA,12*7,(2+length(stats)))) # 12 maanden * (6 regios + 1 landelijk gemiddelde)
      names(tabel) <- c("regio","maand",stats)
      tabel[,1:2]  <- read.table(file.resamp("ALL",var.G,stats[1],sc.G,p.G))[,c(1,3)]
      tabel$maand  <- match(tabel$maand,maanden)
      for(i in 1:length(stats)) {      
        tabel[stats[i]] <- read.table(file.resamp("ALL",var.G,stats[i],sc.G,p.G))[,6]
      }
      # scaling factor 7/6 for W_plus EOC
      if(p == 2085 & sc == "WH") {tabel[-1:-2] <- round(tabel[-1:-2] * 7 / 6,3)}
      
      write.table(tabel,delta.name(var,sc,p),quote=F,row.names=F)
    }
  }
  # decadale voorspelling
  tabel        <- data.frame(matrix(NA,12*7,(2+length(stats)))) # 12 maanden * (6 regios + 1 landelijk gemiddelde)
  names(tabel) <- c("regio","maand",stats)
  tabel[,1:2]  <- read.table(file.resamp("ALL",var.G,stats[1],sc.G,p.G))[,c(1,3)]
  tabel$maand  <- match(tabel$maand,maanden)
  for(i in 1:length(stats)) {
    tabel[stats[i]] <- read.table(file.noresamp("ALL",var.G,stats[i]))[,6]      
  }

  write.table(tabel,delta.name(var,sc="",p=2030),quote=F,row.names=F)
}
  
    





