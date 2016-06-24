# EXAMPLE FILE TRANSFORMATION TOOL KNMI'14

# clean environment and set directory  
rm(list = ls())
graphics.off()

# PRECIPITATION TRANSFORMATION
source("temperatuur_transformatie_KNMI14.R")

ifile="temperatuur_ref"     # input file
ofile="uitvoer.txt"      # output file (DEFAULT="uitvoer.txt")
delta.file=NA            # file containing deltas
                         # if delta.file is not provided (DEFAULT)
                         # KNMI'14 deltas are used

# deltas are derived from KNMI'14 deltas if delta.file is not specified
# following arguments are used
sc="WH"                    # scenario ("GL", "GH", "WL", "WH")
p=2085                     # time horizon (2030, 2050, 2085)
var="tg"                   # temperature characteristic ("tg" = mean,
                           #                             "tn" = min,
                           #                             "tx" = max)
regio.file="stationstabel" # table that links stations to region

temperatuur_transformatie_KNMI14(ifile=ifile,
                                 ofile=ofile,
                                 delta.file=delta.file,
                                 sc=sc,
                                 p=p,
                                 var=var,
                                 regio.file=regio.file)



