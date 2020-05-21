### Preliminary analysis of trade and development

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda')

packs2 <- c("stringr", "reshape2",
            "dplyr", "ggplot2",  "magrittr")

loadPkg(packs)
loadPkg(packs2)

dataPath <- "../"
savePath <- "./"

##data <- read.csv(paste0(dataPath, "paragraphsAndMeta.csv"))

ls()

colnames(data)

head(data)

## pargraph text=data$V@


##### Analysis

processed <- textProcessor(documents=data$V2,
                           metadata=data)

summary(processed) ## 82 docs 5781 word dictionary


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)

##make date into a numeric:


out$meta$numdate <- as.numeric(out$meta$date)


head(out$meta)

##82 docs, 3614 terms, 56784 tokens

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

set.seed(6889)

mod.out <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=20,
               prevalence=~s(numdate)+ docid,
               seed=6889)

 
save(mod.out,
     file=paste0(savePath, "tradDevParaLev20.RData"))

prep <- estimateEffect(c(1:10)~s(numdate) + docid,
                       mod.out,
                       metadata=meta, documents=docs,
                       uncertainty=c("Global"))

save(prep,
     file=paste0(savePath, "estimateEffectParaLevTD20.Rdata"))

##save(meta,docs, vocab,
##     file=paste0(savePath, "tradeDevMetaParaLev.Rdata"))


### select model:
