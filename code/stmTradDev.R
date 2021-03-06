### Preliminary analysis of trade and development

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools', 'tidyr', 'quanteda')

packs2 <- c("stringr", "reshape2",  "dplyr", "ggplot2",  "magrittr")

loadPkg(packs)
loadPkg(packs2)


savePath <- "~/Dropbox/WTO/"

filepath0 <- "../TandDdownloads/ENGLISH/" ##all the docs
filepath1 <- "../TandDdownloads/MeetingNotes/" ## just the meeting notes
## filepath2 <- "../MarketAccessDownloads/ENGLISH/"

files0 <- list.files(path=filepath0,
                    pattern="pdf$")

files1 <- list.files(path=filepath1,
                     pattern="pdf$")

## files2 <- list.files(path=filepath2,
##                     pattern="pdf$")

readpdf <- readPDF(control = list(text = "-layout"))

tradeAndDev <- Corpus(URISource(paste0(filepath0, files0)), 
                   readerControl = list(reader = readpdf))

tdMeetings <- Corpus(URISource(paste0(filepath1, files1)),
                      readerControl = list(reader = readpdf))


## ######tradeAndDev

##"texts.csv" is the file that has the document texts as a
## single field in the columns. Texts.csv in ~/Dropbox/WTO/data/paras
## is broken up by paragraphs

### MetaData #2
 
date <- vector()
files <- vector()


for(i in 1:length(files1)) {
    print(i)
    date[i] <-as.character(pdf_info(pdf=paste0(filepath1,
                                        files1[i]))$created)
    files[i] <- as.character(files1[i])

}

length(text)

metaDataTD <- as.data.frame(cbind(date, files))
dim(metaDataTD)
head(metaDataTD)
class(metaDataTD)

metaData.TD<- separate(metaDataTD, date, into=c("date", "time"), sep=" ")

rownames(metaData.TD) <- metaData.TD$files
metaData.TD$date <- as.Date(metaData.TD$date)

head(metaData.TD)

head(files)

##### Analysis
ls()

tradDevMCorpus <- corpus(tdMeetings)

processed <- textProcessor(tradDevMCorpus,
                           metadata=metaData.TD)

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
               prevalence=~s(numdate),
               seed=6889)


save(mod.out,
     file=paste0(savePath, "tradDev20.RData"))

prep <- estimateEffect(c(1:20)~s(numdate), mod.out,
                       metadata=meta, documents=docs,
                       uncertainty=c("Global"))

save(prep,
     file=paste0(savePath, "estimateEffectTD20.Rdata"))

save(meta,docs, vocab,
     file=paste0(savePath, "tradeDevMeta.Rdata"))


### select model:

mod.sel <- selectModel(documents=docs,
                       vocab=vocab,
                       K=15,
                       prevalence=~s(date),
                       data=meta,
                       seed=6889,
                       runs=50)

plotModels(mod.sel)

