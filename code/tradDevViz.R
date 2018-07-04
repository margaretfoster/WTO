
## Preliminaries

## this document to use STM Browser
## on WTO docs.

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools', 'tidyr', 'quanteda',
           'devtools')

loadPkg(packs)

library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)

savePath <- "~/Dropbox/WTO/"

##load data

load(paste0(savePath, 'tradDev20.RData'))
load(paste0(savePath, 'tradeDevMeta.Rdata'))

ls()

class(meta)
class(docs)

head(meta)

attributes(mod.out)



## stm:
library(stmBrowser)

stmBrowser(mod.out, data=meta, c("numdate"),
                   text="files")

## topic visualizing

load(paste0(savePath, 'estimateEffectTD20.Rdata'))

ls()

labelTopics(mod.out)
