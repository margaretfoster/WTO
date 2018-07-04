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

filepath0 <- "../TandDdownloads/ENGLISH/"
filepath1 <- "../TandDdownloads/MeetingNotes/"
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

## marketAccess <-  Corpus(URISource(paste0(filepath2, files2)),
##                         readerControl = list(reader = readpdf))
## head(files0)

###### tradeAndDev

### MetaData #2
 
date <- vector()
files <- vector()

length(files1) ##82

for(i in 1:length(files1)) {
    print(i)
    date[i] <-as.character(pdf_info(pdf=paste0(filepath1, files1[i]))$created)
    files[i] <- as.character(files1[i])
}

metaDataTD <- as.data.frame(cbind(date, files))
dim(metaDataTD)
head(metaDataTD)
class(metaDataTD)

metaData.TD<- separate(metaDataTD, date, into=c("date", "time"), sep=" ")

rownames(metaData.TD) <- metaData.TD$files
metaData.TD$date <- as.Date(metaData.TD$date)

head(metaData.TD)


##### Analysis
ls()
tradDevMCorpus <- corpus(tradeAndDev)

processed <- textProcessor(tradDevCorpus,
                           metadata=metaData.TD)

summary(processed) ## 267 x 17137 dictionary


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)


##267 docs, 165343 tokens

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

##make date into a numeric:

out$meta$numdate <- as.numeric(out$meta$date)


head(out$meta)

set.seed(6889)

mod.out <- stm(documents=docs,
               vocab=vocab,
               K=20, 
               prevalence=~s(numdate),
               data=meta,
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

