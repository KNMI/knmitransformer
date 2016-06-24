# EXAMPLE FILE TRANSFORMATION TOOL KNMI'14

# clean environment and set directory  
rm(list = ls())
graphics.off()

# PRECIPITATION TRANSFORMATION
source("neerslag_transformatie_KNMI14.R")

ifile="neerslag_ref"     # input file
ofile="uitvoer.txt"      # output file (DEFAULT="uitvoer.txt")
delta.file=NA            # file containing deltas
                         # if delta.file is not provided (DEFAULT)
                         # KNMI'14 deltas are used

# deltas are derived from KNMI'14 deltas if delta.file is not specified
# following arguments are used
sc="WH"                  # scenario ("GL", "GH", "WL", "WH")
p=2085                   # time horizon (2030, 2050, 2085)
scaling="centr"          # scaling subscenario ("lower", "centr" [DEFAULT], "upper")
version="v1.1."          # version of transformation ("v1.1" [DEFAULT], "v1.2")

neerslag_transformatie_KNMI14(ifile=ifile,
                              ofile=ofile,
                              delta.file=delta.file,
                              sc=sc,
                              p=p,
                              scaling=scaling,
                              version=version)



