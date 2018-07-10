
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

head(meta)

dim(meta)

## read in full text in the csv:

ft <- read.csv(paste0(savePath,"data/texts.csv"),
               header=FALSE)

dim(ft) ##81x1 ## NOTE DON'T Inspect--will crash R

meta <- cbind(meta, ft)
colnames(meta) <- c("date","time","files", "numdate", "fulltext")



## stm:
library(stmBrowser)

stmBrowser(mod.out, data=meta, c("numdate"),
                   text="fulltext")

## topic visualizing

load(paste0(savePath, 'estimateEffectTD20.Rdata'))

ls()

labelTopics(mod.out)
