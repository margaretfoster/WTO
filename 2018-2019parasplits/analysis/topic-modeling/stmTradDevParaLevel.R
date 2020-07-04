### Preliminary analysis of trade and development

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

packs2 <- c("stringr", "reshape2",
            "dplyr", "ggplot2",  "magrittr")

loadPkg(packs)
loadPkg(packs2)

dataPath <- "../../"
savePath <- "./"


data <- read.csv(paste0(dataPath,
                        "WTO_TD_NER_Data.csv"),
                        stringsAsFactors=FALSE)

colnames(data)

data$X <- NULL
data$date <- as.Date(data$date)

class(data$date)
hist(data$date,breaks="months")
summary(data$date) ## 4/4/1995- 6/28/2019

##want to add a column for year:
data$year <- format(data$date, "%Y")

colnames(data)
dim(data) ## 8636

## Remove data for "meeting" 28A1, which is an appendix with a
## program of a 2000 seminar on differential treatment of economies

data <- data[!data$docid=="WTCOMTDM28A1",]

dim(data) ##8636x10

## Consolidate the "EU" and "EC" entries with
## European Union and European Communities respectively
## same with UN and United Nations
data[which(data$firstent=="EU"), "firstent"] <- "European Union"
data[which(data$firstent=="EC"), "firstent"] <- "European Communities"
data[which(data$firstent=="UN"), "firstent"] <- "United Nations"

#### Summarize frequency and variety of speakers

data$meetingno <- data$docid
data$meetingno <- as.numeric(gsub(data$meetingno,
                       pattern="WTCOMTDM",
                       replace=""))

load("speakersMeta.Rdata")## dataframe with state income and region information
#####################
###
#### process the data into stm format:

## pargraph text: data$paratext
## metadata colunmns: paranum, date, firstent

##### Analysis

processed <- textProcessor(documents=data$paratext,
                           metadata=data)

summary(processed) ## 8515 documents (paragraphs), 9570  word dictionary


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta) ## 8151 docums, 5839 terms

## Convert "date" field into a numeric counter:

out$meta$numdate <- as.numeric(out$meta$date)

head(out$meta$numdate)
head(out$meta$date)

## rename objects for ease of reference:
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


############################
#### Start running some models
###########################

set.seed(61920)

mod.out.100 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=100, ## start at 100 to get a fine-grained
               ## overview over time
               prevalence= ~ s(numdate) +
                   as.factor(docid)+
                   as.factor(firstent),
               seed=61920)

 

prep.100 <- estimateEffect(c(1:100)~ s(numdate) +
                           as.factor(docid)+
                           as.factor(firstent),
                       mod.out.100,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

### K=75

mod.out.75 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=75, ## step down to 75
               prevalence= ~ s(numdate) +
                   as.factor(docid)+
                   as.factor(firstent),
               seed=61920)



prep.75 <- estimateEffect(c(1:75)~ s(numdate) +
                           as.factor(docid)+
                           as.factor(firstent),
                       mod.out,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

save.image(file="tradDevPara75_100.RData")
