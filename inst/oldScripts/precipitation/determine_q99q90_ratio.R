setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")
rm(list=ls(all=TRUE))

require("reshape2")
require("ggplot2")

source("../station_transform_functions.R")
version="v1.0"
range="19810101-20101231"

# drempel
th=0.1
qq1=0.99
qq2=0.90

ratio     <- function(x) {
  quantile(x[which(x>=th)],qq1)/quantile(x[which(x>=th)],qq2) }

# neerslag
x  <- read.table(paste("../tijdreeksen/",file.name(var="rr",v="v1.0"),sep=""))
x  <- x[which( x[,1]>0),]
mm <- (x[,1]%/%100)%%100

R  <- aggregate(x[,-1],by=list(mm),ratio)

Ra <- round(apply(R,1,median),3)
write.table(cbind(1:12,Ra),"ratio_Q99_Q90.txt",row.name=F,col.name=F)

# figure
R$month <- factor(1:12)
#R$month <- 1:12
X <- melt(R[-1],id="month")[,c(1,3)]
#Y <- data.frame(x=factor(X$month[order(X$month)]),y=X$value[order(X$month)])
#ggplot(X, aes(x=month, y=value)) + geom_boxplot(outlier.shape=NA, fill="red", colour="black")

# define the summary function
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# do it: moet factor zijn
pdf("../../TR349/figures/Pqq_ratios.pdf",
    paper="special",
    width=6,
    height=3)

ggplot(X, aes(month, value)) +
  stat_summary(fun.data = f, geom="boxplot", fill="red") +
  scale_x_discrete(labels=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
                         
dev.off()




