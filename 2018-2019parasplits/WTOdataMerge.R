## script to merge in WTO NER, speaker, and paragraph data

## read in data csvs
## merge according to meeting number and paragraph


dat1 <- read.csv("wtoSecondStageDataParaNum.csv",
                 stringsAsFactors=FALSE)

dat2 <- read.csv("NERwithparanums.csv",
                 stringsAsFactors=FALSE,
                 header=FALSE)

colnames(dat2) <- c("row", "docid", "key", "entities")

##Make sure the same length:
dim(dat1)
dim(dat2)


### take NER-identified entities in the paragraphs and:
##If there is only one entity found, call that the “speaker”
#If there is more than one entity:
##Tag the first as the “speaker”
##Tag the rest of the list as “references”


for(i in 1:dim(dat2)[1]){
    #Find out how long the list of entities is:
    tmp <- unlist(strsplit(dat2$entities[i], split=","))
    ## if empty, keep "speaker" field empty:
    if(length(tmp)==0){
        dat2[i,"speaker"] <- " "
    } else(if(length(tmp)==1){
               dat2[i, "speaker"] <- tmp
               ## if greater than 1, move first to speaker
           }else{
               dat2[i,"speaker"] <- tmp[1] #speaker is first
               dat2[i, "refs"] <- paste(tmp[2:length(tmp)],
                                       collapse= " ") #rest are refa
           })}

## add text:
dat2 <- merge(dat2, dat1, by="key")


## check results:
## discover that the NER doesn't retain order?
salient <- c("speaker", "refs", "text")

dat2[10,salient]
dat2[20, salient] ## order of the extraction is wrong
dat2[43, salient] ## order of the extraction is wrong
dat2[66, salient]

dat2[20, c(salient, "entities")]

