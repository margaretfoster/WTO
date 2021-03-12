### Take paragraph text + references
## extract a subset to check NER error rates


rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs2 <- c("tidyr", "reshape2", "igraph",
            "dplyr", "ggplot2")

metadatainfo <- c("wbstats")

loadPkg(c(packs2, metadatainfo))

###########################
#### Declare Paths
##########################

if(Sys.info()['user']=="Ergane"){## desktop
    dataPathDesktop <- "~/Dropbox/WTO/data/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
    load(paste0(dataPathDesktop,
                        "speakersMeta.Rdata"))
}else{ ## any other machine
    dataPathDesktop <- "../../data/"
    print(paste0("On remote, data path is ", dataPathDesktop))
    load(paste0(dataPathDesktop,
                        "CTD-checked-speakers.Rdata"))
}

class(speakers.meta)## data.frame
dim(speakers.meta) ## 8659 x 27

class(metadatainfo)
colnames(speakers.meta)

subcols <-c("docid","firstent","para" ,
         "doc", "paranum", "paratext",
         "ents", "date", "meetingno",
         "pid", "year","iso3c",
         "country", "region", "income_level_iso3c")

data <- speakers.meta[, subcols]

dim(data) ##8659 x 15

colnames(data)[which(colnames(data)=="firstent")] <- "speaker"

colnames(data)[which(colnames(data)=="ents")] <- "ner"

colnames(data)

data[5440,]

## Pull a random sample of rows to check NER:


### 12/11/20: Got back the first sample from RAs,
### passing them another 100.
## Since I've set a seed, going to pull 400 rows and segment
## into 4 100-row sets:

set.seed(6889)
samp100 <- sample(1:nrow(data), size=400, replace=F)

## a second set:
sampset2 <- sample(1:nrow(data), size=400, replace=F)

## see if any overlap in sets 1 and 2
length(intersect(sampset2, samp100)) ##20
s.set2 <- setdiff(sampset2, samp100) ### elements of set two !in set 1

head(samp100)

datasamp <- data[samp100,]

dim(datasamp) ## 400x 15

write.csv(datasamp[1:100,],
          file=paste0(dataPathDesktop,
                      "WTO100RowsForNERTest.csv"))

write.csv(datasamp[101:200,],
          file=paste0(dataPathDesktop,
                      "WTO100RowsForNERTestSet2.csv"))

write.csv(datasamp[201:300,],
          file=paste0(dataPathDesktop,
                      "WTO100RowsForNERTestSet3.csv"))

write.csv(datasamp[301:400,],
          file=paste0(dataPathDesktop,
                      "WTO100RowsForNERTestSet4.csv"))


######### Write out second pull:

length(s.set2) ## 380, check

data.set2 <- data[s.set2,]
dim(data.set2) ## 380x 15

write.csv(data.set2[1:100,], ## 5th set of 100 rows
          file=paste0(dataPathDesktop,
                      "WTO100RowsForNERTestSet5.csv"))
