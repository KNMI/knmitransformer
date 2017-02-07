# TRANSFORMATION TOOL KNMI'14 applied to q De Bilt (260) 1901-2014

# clean environment and set directory  
rm(list = ls())
graphics.off()

# GLOBAL_RADIATION TRANSFORMATION
source("straling_transformatie_KNMI14.R")

ifile="DeBilt_q_1901-2014_detrended.dat"                                                                                         # input file  used by transformation R-script               
                                                                                                                                 # NOTE this file serves as input for the transformation, 
                                                                                                                                 # but it is created at the beginning of this script from 
                                                                                                                                 # file Gedetrende_reeksen_T_en_Q_DeBilt.prn to which a header is added 
                                                                                                                                 # with information about the De Bilt series (see below).
ofile="uitvoer.txt"                                                                                                              # output file (DEFAULT="uitvoer.txt")
delta.file=NA                                                                                                                    # file containing deltas
                                                                                                                                 # if delta.file is not provided (DEFAULT) KNMI'14 deltas are used

# deltas are derived from KNMI'14 deltas if delta.file is not specified
# following arguments are used
sc="WH"                                                                                                                          # scenario ("GL", "GH", "WL", "WH")
p=2085                                                                                                                           # time horizon (2030, 2050, 2085)

#Relevant input parameters for 'stralings_transformatie_KNMI14':
#straling_transformatie_KNMI14(ifile=ifile,
#                              ofile=ofile,
#                              delta.file=delta.file,
#                              sc=sc,
#                              p=p)
#

 DeBilt_tg_and_q_detrended <- read.table("./Gedetrende_reeksen_T_en_Q_DeBilt.prn", header = F, skip = 2)                         # Original file containing the relevant data

 dim_names <- list(2)
 dim_names[[1]] <- 1:dim(DeBilt_tg_and_q_detrended)[1]
 dim_names[[2]] <- c("date", "q.detrend.2014", "q.detrend.1995")
 DeBilt_q <- array(data = NA, dim = c(dim(DeBilt_tg_and_q_detrended)[1], 3), dimnames = dim_names)

 DeBilt_q[,1] <- DeBilt_tg_and_q_detrended[,1]           #dates
#DeBilt_q[,2] <- DeBilt_tg_and_q_detrended[,3]           #glob. radation detrended to 2014
#DeBilt_q[,3] <- DeBilt_tg_and_q_detrended[,5]           #glob. radation detrended to 1995
 DeBilt_q[,2] <- 10*DeBilt_tg_and_q_detrended[,3]        #glob. radation detrended to 2014 and converted from J/cm2 to kJ/m2
 DeBilt_q[,3] <- 10*DeBilt_tg_and_q_detrended[,5]        #glob. radation detrended to 1995 and converted from J/cm2 to kJ/m2

 DeBilt_q <- as.data.frame(DeBilt_q)                     #nodig ivm controle over "geformateerd" wegschrijven naar file
 DeBilt_q[,1] <- as.integer(DeBilt_q[,1])                #nodig icm "nsmall=1" bij write.table (zie onder)

sink(ifile)    
#writeLines("# Daily global radiation [J/cm2] De Bilt 1901-2014 (station 260) of Royal Netherlands Meteorological Institute (KNMI)")   # comment
 writeLines("# Daily global radiation [kJ/m2] De Bilt 1901-2014 (station 260) of Royal Netherlands Meteorological Institute (KNMI)")   # comment
 writeLines("# Detrended to 2014 (second column, Versteeg/STOWA, 2015) and detrended to 1995 (third column, Versteeg/STOWA, 2015)")    # comment
# header:
 writeLines("00000000      260      260")       #stationsnummer
 writeLines("00000000  141.000  141.000")       #RD x-coordinaat [in km]
 writeLines("00000000  457.000  457.000")       #RD y-coordinaat [in km]
 writeLines("00000000    5.183    5.183")       #longitude
 writeLines("00000000   52.100   52.100")       #latidude;    NB deze heeft "straling_transformatie_KNMI14.R" nodig en verwacht deze in de 5e regel van de header!
#write.table(format(DeBilt_q, width = 8), file = "", row.names = F, col.names = T, quote = F)                                   # data
 write.table(format(DeBilt_q, width = 8, nsmall=1), file = "", row.names = F, col.names = F, quote = F)                         # data
sink()

# Loops for 2050 and 2085

for (p in c(2050, 2085)) {
  for (sc in c("GL", "GH", "WL", "WH")) {
 
    ofile <- paste(substr(ifile, 1, nchar(ifile)-4), "_", p, "_", sc, ".dat", sep = "") 
    print(ofile)

    straling_transformatie_KNMI14(ifile=ifile,
                                  ofile=ofile,
                                  delta.file=delta.file,
                                  sc=sc,
                                  p=p)

  } #endfor [sc] 
} #endfor [p] 


# For 2030

 ofile <- paste(substr(ifile, 1, nchar(ifile)-4), "_2030___", ".dat", sep = "") 
 print(ofile)

 straling_transformatie_KNMI14(ifile=ifile,
                               ofile=ofile,
                               delta.file=delta.file,
                               sc=NA,
                               p=2030)



# END