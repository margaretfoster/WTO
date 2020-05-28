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


data <- read.csv(paste0(dataPath,
                        "WTO_TD_NER_Data.csv"),
                        stringsAsFactors=FALSE)

colnames(data)

data$X <- NULL
data$date <- as.Date(data$date)

class(data$date)
hist(data$date,breaks="months")

## pargraph text=data$paratext

## metadata colunmns: paranum, date, firstent

##### Analysis

processed <- textProcessor(documents=data$paratext,
                           metadata=data)

summary(processed) ## 5781 documents, 7249 word dictionary


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)

## 5781 documents, 4605 terms


##make date into a numeric:

class(out$meta$date)## factor

out$meta$numdate <- as.numeric(out$meta$date)

head(out$meta$numdate)
head(out$meta$date)

##82 docs, 3614 terms, 56784 tokens

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

colnames(meta)

head(meta$doc)
head(meta$docid)

set.seed(6889)


mod.out <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=100, ## start at 100 to get a fine-grained
               ## overview over time
               prevalence= ~ s(numdate) +
                   as.factor(docid)+
                   as.factor(firstent),
               seed=6889)

 

prep <- estimateEffect(c(1:100)~ s(numdate) +
                           as.factor(docid)+
                           as.factor(firstent),
                       mod.out,
                       metadata=meta, documents=docs,
                       uncertainty=c("Global"))

save.image(file=paste0(savePath, "tradDevParaLev100.RData"))
