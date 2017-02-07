# TRANSFORMATION TOOL KNMI'14 applied to tg De Bilt (260) 1901-2014

# clean environment and set directory  
rm(list = ls())
graphics.off()

# TEMPERATURE TRANSFORMATION
source("temperatuur_transformatie_KNMI14.R")

ifile="DeBilt_tg_1901-2014_detrended.dat"                                                                                        # input file used by transformation R-script                
                                                                                                                                 # NOTE this file serves as input for the transformation, 
                                                                                                                                 # but it is created at the beginning of this script from 
                                                                                                                                 # file Gedetrende_reeksen_T_en_Q_DeBilt.prn to which a header added 
                                                                                                                                 # with information about the De Bilt series (see below).
ofile="uitvoer.txt"                                                                                                              # output file (DEFAULT="uitvoer.txt")
delta.file=NA                                                                                                                    # file containing deltas
                                                                                                                                 # if delta.file is not provided (DEFAULT)
                                                                                                                                 # KNMI'14 deltas are used

# deltas are derived from KNMI'14 deltas if delta.file is not specified
# following arguments are used
sc="WH"                                                                                                                          # scenario ("GL", "GH", "WL", "WH")
p=2085                                                                                                                           # time horizon (2030, 2050, 2085)
var="tg"                                                                                                                         # temperature characteristic ("tg" = mean,
                                                                                                                                 #                             "tn" = min,
                                                                                                                                 #                             "tx" = max)
regio.file="stationstabel"                                                                                                       # table that links stations to region

#Relevant input parameters for 'temperatuur_transformatie_KNMI14':
#temperatuur_transformatie_KNMI14(ifile=ifile,
#                                 ofile=ofile,
#                                 delta.file=delta.file,
#                                 sc=sc,
#                                 p=p,
#                                 var=var,
#                                 regio.file=regio.file)


 DeBilt_tg_and_q_detrended <- read.table("./Gedetrende_reeksen_T_en_Q_DeBilt.prn", header = F, skip = 2)

 dim_names <- list(2)
 dim_names[[1]] <- 1:dim(DeBilt_tg_and_q_detrended)[1]
 dim_names[[2]] <- c("date", "tg.detrend.2014", "tg.detrend.1995")
 DeBilt_tg <- array(data = NA, dim = c(dim(DeBilt_tg_and_q_detrended)[1], 3), dimnames = dim_names)

 DeBilt_tg[,1] <- DeBilt_tg_and_q_detrended[,1]                                                                                  #dates
 DeBilt_tg[,2] <- DeBilt_tg_and_q_detrended[,2]                                                                                  #tg detrended to 2014
 DeBilt_tg[,3] <- DeBilt_tg_and_q_detrended[,4]                                                                                  #tg detrended to 1995

 DeBilt_tg <- as.data.frame(DeBilt_tg)                                                                                           #nodig ivm controle over "geformateerd" wegschrijven naar file

sink(ifile)    
 writeLines("# Daily Tg [deg. C] De Bilt 1901-2014 (station 260) of Royal Netherlands Meteorological Institute (KNMI)")                   # comment
 writeLines("# Detrended to 2014 (second column, Versteeg/STOWA, 2015) and detrended to 1995 (third column, Versteeg/STOWA, 2015)")                           # comment
# header:
 writeLines("00000000      260      260")       #stationsnummer;  NB deze heeft 'temperatuur_transformatie_KNMI14' nodig voor het bepalen van de regio obv de file "stationstabel"
 writeLines("00000000  141.000  141.000")       #RD x-coordinaat [in km]
 writeLines("00000000  457.000  457.000")       #RD y-coordinaat [in km]
 writeLines("00000000    5.183    5.183")       #longitude
 writeLines("00000000   52.100   52.100")       #latidude
#write.table(format(DeBilt_tg, width = 8), file = "", row.names = F, col.names = T, quote = F)                                   # data
 write.table(format(DeBilt_tg, width = 8), file = "", row.names = F, col.names = F, quote = F)                                   # data
sink()

# Loops for 2050 and 2085

for (p in c(2050, 2085)) {
  for (sc in c("GL", "GH", "WL", "WH")) {
 
    ofile <- paste(substr(ifile, 1, nchar(ifile)-4), "_", p, "_", sc, ".dat", sep = "") 
    print(ofile)

    temperatuur_transformatie_KNMI14(ifile=ifile,
                                     ofile=ofile,
                                     delta.file=delta.file,
                                     sc=sc,
                                     p=p,
                                     var="tg",
                                     regio.file="stationstabel")

  } #endfor [sc] 
} #endfor [p] 


# For 2030

 ofile <- paste(substr(ifile, 1, nchar(ifile)-4), "_2030___", ".dat", sep = "") 
 print(ofile)

 temperatuur_transformatie_KNMI14(ifile=ifile,
	                          ofile=ofile,
                                  delta.file=delta.file,
                                  sc=NA,
                                  p=2030,
                                  var="tg",
                                  regio.file="stationstabel")



# END