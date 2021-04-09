
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 
           'tidyr', 'quanteda')

loadPkg(c(packs))

########################
## Declare Data Paths
#########################


if(Sys.info()['user']=="Ergane"){ ## my desktop
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{
    if(Sys.info()['user']=="Promachos"){ ##my laptop
        dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
        print(paste0("The datapath is: ", dataPathDesktop))
    }else{ ## else look in ~/WTO/
        dataPathDesktop <- ""
        print(paste0("The datapath is: ", dataPathDesktop))
    }}

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info
load(paste0(dataPathDesktop,"K24-25Model.Rdata"))
load(paste0(dataPathDesktop,"K85model.Rdata"))
load(paste0(dataPathDesktop,"K100model.Rdata"))


#################
### Play with the topic models
### 4/9/21
######
#####################

library(stm)

stm::plot.STM(x=mod.out.25,
     type="perspectives",
     topics=23,
     covarlevels=c("HIC","LIC"),
     )

ls()

