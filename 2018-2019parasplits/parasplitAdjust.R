 
## script breaks the WTO full-text documents into paragraphs
## note though that it will also break "pararaphs" after a page break

rm(list=ls())

## working with the MeetingNotesCopy directory
## to keep titles the same

## load libraries:prelimsetup.R
source('../code/prelimsetup.R')

## Pass in directory locations
readPath1 <- "../2018-2019Data/"
outPath1 <- "/splits/"


files <- list.files(readPath1, pattern="^[WTCOMTDM]")

files


allparas <- data.frame()

for(j in files){
     print(j)
     dat <- read_file(paste0(readPath1, j))
     ## this data I hand split to insert a && at the start of
     ## a paragraph that changed the speaker
     dat <- strsplit(dat, split='&&')
     dat <- as.data.frame(dat,
                          strings.as.factors=FALSE,
                          col.names="text")
     dat$doc <- j 
     ## need to add a paragraph counter
     dat$paranum <- 1:dim(dat)[1]
     dat$key <- paste0(dat$doc, "_para", dat$paranum) ## data row with doc + paranumber
     print("dat made")
     ##print(head(dat))

     allparas <- rbind(allparas, dat)

     ##next development step:
     ##rather than sending out to .txt,
     ##write into a dataframe with columns for
     ## paragraphs and doc names.
     
     ## for(i in 1:dim(dat)[1]){
     ##  write(as.character(dat[i,1]),
     ##  file=paste0(outPath1, "para.", i, ".doc.", j ))
     ##    }
}

dim(allparas)

write.csv(allparas,
          file="wtoSecondStageDataParaNum.csv")
