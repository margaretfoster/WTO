## Second pass through, pulling about 500
## paragraphs to hand-tag

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('stm', 'ggplot2',
           'dplyr','tidyr')
loadPkg(packs)

ls()

#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane" |
   Sys.info()['user']=="Promachos"){ ## if on my own machines look in Dropbox
    print(Sys.info()['user'])
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
## Data 1: K=10 model on Program subset
#############################
dat.for.pid <- read.csv("secondtaggingSet.csv")

dat.for.pid$X <- NULL

dim(dat.for.pid)

colnames(dat.for.pid)
dat.for.pid$num <- 1:dim(dat.for.pid)[1]

tail(dat.for.pid$pid)

dat.for.pid[which(dat.for.pid$pid==4570),] ## 253/500

dat.for.pid[which(dat.for.pid$pid==2020145),] # 495/500


dat.for.pid[500,]
