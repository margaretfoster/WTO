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

Ks=seq(from=4, to=26,by=2)

mod.tdsk <- searchK(documents=docs,
                       vocab=vocab,
                       K=Ks,
                       prevalence=~s(numdate),
                       data=meta,
                       seed=6889)

save(mod.tdsk,
     file=paste0(savePath,"tradevsearchk.Rdata"))

