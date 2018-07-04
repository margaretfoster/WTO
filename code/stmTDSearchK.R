### Preliminary analysis of trade and development

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools', 'tidyr', 'quanteda')

loadPkg(packs)

savePath <- "~/Dropbox/WTO/"


load(paste0(savePath, "tradeDevMeta.Rdata"))


### searchk

mod.tdsk <- selectModel(documents=docs,
                       vocab=vocab,
                       K=seq(from=5, to=25,by=5),
                       prevalence=~s(date),
                       data=meta,
                       seed=6889)

save(mod.tdsk,
     file=paste0(savePath,"tradevsearchk.Rdata"))

