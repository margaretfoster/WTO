
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

savePath <- "~/Dropbox/WTO/rdatas/"

##load data

load(paste0(savePath, '/rdatas/tradDevParaLev20.RData'))
load(paste0(savePath, '/rdatas/tradeDevMetaParaLev.Rdata'))

ls()

head(meta)

dim(meta)

## read in full text in the csv:

## stm:
library(stmBrowser)

stmBrowser(mod.out, data=meta, c("numdate", "docid"),
                   text="V2")

## topic visualizing

##devtools::install_github('methodds/stminsights')

## label topics: by default prints 3 most representative
## documents for each topic
load(paste0(savePath, 'estimateEffectParaLevTD20.Rdata'))

ls()

labelTopics(mod.out)

pdf(file=paste0(savePath, "fredPara20.pdf"))
plot(mod.out, type="summary", labeltype="frex")
dev.off()
    
plot(mod.out, type="perspectives",
     topics=c(4, 8))



findThoughts(mod.out,
             texts=meta$V2,
             topics=c(4, 13, 14, 10, 17,6))

## Plot estimate effect

class(mod.out)
class(prep)

colnames(meta)
