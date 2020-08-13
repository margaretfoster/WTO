### Take the adjacency network
### Convert into a country-year or
## (ideally) country-meeting count of ingoing or outgoing ties

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs2 <- c("tidyr", "reshape2", "igraph",
            "dplyr", "ggplot2")

loadPkg(packs2)

###########################
#### Declare Paths
##########################

if(Sys.info()['user']=="Ergane"){## desktop
    dataPathDesktop <- "~/Dropbox/WTO/data/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine
    dataPathDesktop <- "~/Dropbox/WTO/"
    print(paste0("On remote, data path is ", dataPathDesktop))
}


data <- read.csv(paste0(dataPathDesktop,
                        "WTO_TDM_Reference_AjM.csv"),
                        stringsAsFactors=FALSE)

colnames(data)

## need "source" to have a different name

colnames(data)[which(colnames(data)=="source")] <- "sender"

colnames(data)


##########################
##Data overview
##########################


dim(data) ## 3478 x 13. No non-state

length(unique(data$sender)) ##104
length(unique(data$target)) ## 145


##############################
## Sidenote to get a feel for who is
## overall most active
freqSender <- as.data.frame(table(data$sender))

freqTarget <- as.data.frame(table(data$target))

freqSender <- freqSender[order(freqSender$Freq),]
freqTarget <- freqTarget[order(freqTarget$Freq),]


tail(freqSender)
## Top 6 senders (all years):
## US at 439, India 326, EU 281, Egypt 202, China 134, Canada 114

tail(freqTarget)
## Top 6 targets (all years):
## EU at 337, India at 334, Egypt at 244, China at 182
## US at 143 and Hong Kong at 143


head(freqSender)

head(freqTarget)

colnames(data)
head(data$doc_id)
head(data)

### what are the most frequent edges?

edgeL <- data[,c("sender", "target", "doc_id", "year")]

head(edgeL)

## standard edge format:

tmp <- paste0(edgeL$sender, "-", edgeL$target)

edgeFreq <- as.data.frame(table(tmp))

edgeFreq <- edgeFreq[order(edgeFreq$Freq),]
colnames(edgeFreq) <- c("S-T", "Frequency")

dim(edgeFreq) ##1362 unique ties

## Ten most frequent:

edgeFreq[1352:1362,]

#######################################
## Now want a country-meeting dataset of
## references sent and references recieved
######################################

colnames(data)

data$doc_id <- as.factor(data$doc_id)


datS <- data %>% group_by(doc_id, sender) %>% tally()

datT <- data %>% group_by(doc_id, target) %>% tally()

dim(datS) ##1270x3
dim(datT) ## 1487x3

colnames(datS)[which(colnames(datS)=='n')] <- 'numsends'
colnames(datT)[which(colnames(datT)=='n')] <- 'numrefs'

head(datS)
head(datT)


print(datS[which(datS$sender=="EU"),], n=104)
print(datT[which(datT$target=="EU"),], n=104)


#### want a dataframe with
### C1: Meeting ID
### C2: State name
### C3: Number of sends
### C4: Number of mentions

length(unique(data$sender))
length(unique(data$target)) ## 145
length(unique(data$doc_id)) ## 105

## SO: meeting-country level df should be 15225 x 2

## build it slow with for-loop:
mcdat <- data.frame()

for(m in unique(data$doc_id)){
    tmp <- cbind(unique(data$target), m)
    mcdat <- rbind(mcdat, tmp)
}

colnames(mcdat) <- c("country", "docid")

### now, merge in country-level tallies:
## sender=datS, target= datT

head(datT)
head(datS)

mcdat2 <- merge(x=mcdat,
                y=datT,
                by.x= c('docid', 'country'),
                by.y= c('doc_id', 'target'),
                all.x=TRUE)

mcdat2 <- merge(x=mcdat2,
                y=datS,
                by.x= c('docid', 'country'),
                by.y= c('doc_id', 'sender'),
                all.x=TRUE)

## Replace NA with 0:

mcdat2[is.na(mcdat2$numrefs), 'numrefs'] <- 0
mcdat2[is.na(mcdat2$numsends), 'numsends'] <- 0

head(mcdat2)

mcdat2$delta <- mcdat2$numsends-mcdat2$numrefs

mcdat2$delttype <- NA

ifelse(mcdat2$delta==0,##if out and in are the same
       mcdat2$delttype <- "SameOutIn",
       ifelse(mcdat2$delta>0,
              mcdat2$delttype <- "MoreOut",
              ifelse(mcdat2$delta<0,
                     mcdat2$delttype <- "MoreIn")))

mcdat2[which(mcdat2$delta==0), 'delttype'] <- "SameOutIn"
mcdat2[which(mcdat2$delta>0), 'delttype'] <- "MoreOut"
mcdat2[which(mcdat2$delta<0), 'delttype'] <- "MoreIn"
mcdat2[which(mcdat2$numrefs==0 &
             mcdat2$numsends==0), 'delttype'] <- 'NoOutNoIn'
 
table(mcdat2$delttype)
## MoreIn = 1013
## More Out = 842
## SameOutIn = 181
## NoOutNoIn = 13189

save(mcdat2,
     file=paste0(dataPathDesktop, "country-meetingRefActivitySums.Rdata"))



 
