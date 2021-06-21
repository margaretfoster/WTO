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


## Save the "out" data object; models; and estimated effects
## Discovered that I can compare 2 models

save(out, mod.out.20, prep.20,
     mod.out.21, prep.21,
     file=paste0(dataPathDesktop,"K20-22Model.Rdata"))


save(out,
     mod.out.22, prep.22,
     mod.out.23, prep.23,
     file=paste0(dataPathDesktop,"K22-22Model.Rdata"))

save(out,
     mod.out.24, prep.24,
     mod.out.25, prep.25,
     file=paste0(dataPathDesktop,"K24-25Model.Rdata"))

save(out,
     mod.out.85, prep.85,
     file=paste0(dataPathDesktop,"K85Model.Rdata"))


save(out,
     mod.out.100, prep.100,
     file=paste0(dataPathDesktop,"K100Model.Rdata"))


