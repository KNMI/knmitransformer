# EXAMPLE FILE TRANSFORMATION TOOL KNMI'14

# clean environment and set directory  
rm(list = ls())
graphics.off()

# PRECIPITATION TRANSFORMATION
source("neerslag_transformatie_KNMI14.R")
#source("neerslag_transformatie_KNMI14_jules.R")        #Only difference is adapted format of output data

#ifile="neerslag_ref"                                  # input file
ifile="DeBilt_v6.dat"                                  # input file                 
                                                       # NOTE this file serves as input for the transformation, but it is created at the beginning of this script from 
                                                       # file DeBilt_v6.txt to which a header added with information about the De Bilt series (see below).
ofile="uitvoer.txt"                                    # output file (DEFAULT="uitvoer.txt")
delta.file=NA                                          # file containing deltas
                                                       # if delta.file is not provided (DEFAULT) KNMI'14 deltas are used

# deltas are derived from KNMI'14 deltas if delta.file is not specified
# following arguments are used
sc="WH"                                                # scenario ("GL", "GH", "WL", "WH")
p=2085                                                 # time horizon (2030, 2050, 2085)
scaling="centr"                                        # scaling subscenario ("lower", "centr" [DEFAULT], "upper")
version="v1.1."                                        # version of transformation ("v1.1" [DEFAULT], "v1.2")

#neerslag_transformatie_KNMI14(ifile=ifile,
#                              ofile=ofile,
#                              delta.file=delta.file,
#                              sc=sc,
#                              p=p,
#                              scaling=scaling,
#                              version=version)


 DeBilt_Pdaily_Theo <- read.table("./DeBilt_v6.txt", header = F, skip = 1)           #Original data file

 dim_names <- list(2)
 dim_names[[1]] <- 1:dim(DeBilt_Pdaily_Theo)[1]
 dim_names[[2]] <- c("date", "precip", "precip.hom", "precip.hom.detrend.1995")
 DeBilt_Pdaily <- array(data = NA, dim = c(dim(DeBilt_Pdaily_Theo)[1], 4), dimnames = dim_names)

 DeBilt_Pdaily[,1]   <- DeBilt_Pdaily_Theo[,2]*10000 + DeBilt_Pdaily_Theo[,3]*100  + DeBilt_Pdaily_Theo[,4]
 DeBilt_Pdaily[,2] <- DeBilt_Pdaily_Theo[,5]   #raw data
 DeBilt_Pdaily[,3] <- DeBilt_Pdaily_Theo[,6]   #homogenized precip.
 DeBilt_Pdaily[,4] <- DeBilt_Pdaily_Theo[,8]   #homogenized precip. detrended to 1995 (is REFERENCE for KNMI14 scenarios)

 DeBilt_Pdaily <- as.data.frame(DeBilt_Pdaily)  #nodig ivm controle over "geformateerd" wegschrijven naar file

sink(ifile)    
 writeLines("# Daily precipitation sums De Bilt (station 550, tapped 8.00 UT) of Royal Netherlands Meteorological Institute (KNMI)") # comment
 writeLines("# Original (1906-2014), homogenised (Brandsma et al., 2014) and homogenised + detrended_to_1995 (Brandsma/STOWA, 2015)")# comment
 writeLines("00000000      550      550      550")      # station numer
 writeLines("00000000  141.000  141.000  141.000")      # RD x-coordinate [in km]
 writeLines("00000000  457.000  457.000  457.000")      # RD y-coordinate [in km]
 writeLines("00000000    5.178    5.178    5.178")      # longitude
 writeLines("00000000   52.101   52.101   52.101")      # latitude
#write.table(format(DeBilt_Pdaily, width = 8), file = "", row.names = F, col.names = T, quote = F)                                   # data
 write.table(format(DeBilt_Pdaily, width = 8), file = "", row.names = F, col.names = F, quote = F)                                   # data
sink()

# Loops for 2050 and 2085

for (p in c(2050,2085)) {
  for (sc in c("GL","GH","WL","WH")) {
    for (scal in c("centr", "lower", "upper")) {

      ofile <- paste(substr(ifile, 1, nchar(ifile)-4), "_", p, "_", sc, "_", scal, ".dat", sep = "") 
      print(ofile)

      neerslag_transformatie_KNMI14(ifile=ifile,
                                    ofile=ofile,
                                    delta.file=delta.file,
                                    sc=sc,
                                    p=p,
#                                   scaling=scaling,
                                    scaling=scal,
                                    version=version)

    } #endfor [scal] 
  } #endfor [sc] 
} #endfor [p] 


# Loop for 2030

for (p in c(2030)) {
#  for (sc in c("GL","GH","WL","WH")) {             # For 2030 sc is ignored/unused
    for (scal in c("centr", "lower", "upper")) {

      ofile <- paste(substr(ifile, 1, nchar(ifile)-4), "_", p, "____", scal, ".dat", sep = "") 
      print(ofile)

      neerslag_transformatie_KNMI14(ifile=ifile,
                                    ofile=ofile,
                                    delta.file=delta.file,
                                    sc=NA,
                                    p=p,
#                                   scaling=scaling,
                                    scaling=scal,
                                    version=version)

    } #endfor [scal] 
#  } #endfor [sc] 
} #endfor [p] 


# END