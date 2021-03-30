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
           'tidyr', 'quanteda',
           'stminsights', "rsconnect")

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## desktop
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{
    if(Sys.info()['user']=="Promachos"){ ## laptop
        dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
        print(paste0("The datapath is: ", dataPathDesktop))
    }else{ ## else look in ~/WTO/
        dataPathDesktop <- "../../"
        print(paste0("The datapath is: ", dataPathDesktop))
    }}

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop, ## Models
            "tradDevPara_15to25M113.RData"))

load(paste0(dataPathDesktop, ## Estimated effects
            "tradDevParaM1to113-Pandemic.RData"))

ls()


summary(mod.out.20)
summary(mod.out.21)
summary(mod.out.22)
summary(mod.out.23)
summary(mod.out.24)
summary(mod.out.25)


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


pdf(file="K15-85-100ComparisonFrex.pdf")
par(mfrow=c(2, 4))
plot(mod.out.85,
     topics=c(1:21), 
     main ="85 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.85,
     topics=c(22:42), 
     main ="85 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.85,
     topics=c(43:63), 
     main ="85 Topic Model (frex)",
     n=5,
     labeltype = c("frex"))
plot(mod.out.85,
     topics=c(64:85), 
     main ="85 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.100,
     topics=c(1:25),
     main ="100 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.100,
     topics=c(26:50),
     main ="100 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.100,
     topics=c(50:75),
     main ="100 Topic Model (frex)", n=5,
     labeltype = c("frex"))
plot(mod.out.100,
     topics=c(76:100),
     main ="100 Topic Model (frex)", n=5,
     labeltype = c("frex"))
dev.off()


ls()

## Remove the low models:

## Save the "out" data object; models; and estimated effects
save.image(file=paste0(dataPathDesktop,
               "stmM1to113ComparisonK20to25-85-100.Rdata"))



