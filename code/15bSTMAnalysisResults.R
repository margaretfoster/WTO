## This script to
## evaluate the output from
## the stm searchK


rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

##load("WTOSearchKwide.Rdata") ## 
##load("WTOSearchkWideTo50.Rdata") ## K= 15-50
load("WTOSearchK40to50by3.Rdata")

class(search.wide)

plot(search.wide) ## 40- 50 by 3
plot(search.wide2) ## 15-50

## Save for future reference:

pdf(file="wtoSearchK5to30.pdf")
plot(search.wide)
dev.off()


pdf(file="wtoSearchK15to50.pdf")
plot(search.wide2)
dev.off()

## No obvious selection point for each of the above
## only one plateau point for held-out liklihood
## (between 20-30 topics, but range is pretty
## narrow across whole sweep from 15-50
## (between -6.12 and -6.04)


################
