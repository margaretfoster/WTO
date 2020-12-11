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
colnames(data)[which(colnames(data)=="firstent")] <- "ner"

colnames(data)

data[5440,]

## Pull a random sample of rows to check NER:

set.seed(6889)


samp100 <- sample(1:nrow(data), size=100, replace=F)

datasamp <- data[samp100,]

write.csv(datasamp,
          file=paste0(dataPathDesktop,
                      "WTO100RowsForNERTest.csv"))

##########################
##Extract paragraph metadata
## paragraph text; references
##########################
##################################
### remove info other than the edges
