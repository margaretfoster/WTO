### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda')

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop, "tradDevPara_15to25M113.RData"))

ls()

summary(mod.out.15)

summary(mod.out.20)

summary(mod.out.22)

summary(mod.out.23)
summary(mod.out.24)
summary(mod.out.25)

load(paste0(dataPathDesktop, "tradDevParaM1to113-Pandemic.RData"))

ls()

summary(mod.out.21)

## plot a comparison of the topic prevalences and top words



library("ggthemes")
library(tidyverse)

ls()
attributes(mod.out.21)

dim(mod.out.21$theta) ## 8853 x 21 ## documents x topic proportions
class(mod.out.21$beta) ## list

pdf(file="K20to25M113ComparisonProb.pdf")
par(mfrow=c(2, 3))
plot(mod.out.20, main = "20 Topic Model (prob) ", n=5,
     labeltype = c("prob"))
plot(mod.out.21, main ="21 Topic Model (prob)", n=5,
     labeltype = c("prob"))
plot(mod.out.22, main ="22 Topic Model (prob)", n=5,
     labeltype = c("prob"))
plot(mod.out.23, main = "23 Topic Model (prob)", n=5,
     labeltype = c("prob"))
plot(mod.out.24, main = "24 Topic Model (prob)", n=5,
     labeltype = c("prob"))
plot(mod.out.25, main = "25 Topic Model (prob)", n=5,
     labeltype = c("prob"))
dev.off()


### Frex
pdf(file="K20to25M113ComparisonFrex.pdf")
par(mfrow=c(2, 3))
plot(mod.out.20, main = "20 Topic Model (frex) ", n=5,
     labeltype = c("frex"))
plot(mod.out.21, main ="21 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.22, main ="22 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.23, main = "23 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.24, main = "24 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.25, main = "25 Topic Model (frex)", n=5,
     labeltype = c("frex"))
dev.off()


### Perspectives shows topics according to a covariate:
