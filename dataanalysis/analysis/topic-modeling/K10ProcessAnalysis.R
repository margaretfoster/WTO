### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## This specification: Model covariates by year with faction
## content covariates

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats",
           "ggplot2", "gt")

loadPkg(packs)


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
#############################

load("ProcessSubSetstmYearStemmedInc.Rdata")

ls()

## want: mod.out.2.inc
## mod.out.2.fac

## Figure A3: Panel grid of each K=10 topic
## in the 
plot(mod.process.themeinc)

plot.estimateEffect(prep.process.themeinc,  #Topic proportions in Rep. debates
                    covariate="year",
                    model=mod.process.themeinc,
                    topics=prep.process.themeinc$topics[1],
                    method="continuous",
                    xlab="Year",
                    ylab="Expected Topic Proportions",
                    main="Process Topic One")
