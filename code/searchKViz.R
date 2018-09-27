
rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm')

loadPkg(packs)

dataPath <- "./"
savePath <- "../visuals/"

## visualize the trade and development
                                        # search K
load(paste0(dataPath, "tradDevKsearchParas1025.Rdata" ))

ls()

plot.searchK(second)
