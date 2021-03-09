 
## script breaks the WTO full-text documents into paragraphs
## note though that it will also break "pararaphs" after a page break
## The data looks for [manually-inserted] "&&" which signals a paragraph break

## I manually split the data because the original formatting has been extremely difficult to put together a consistent rule to use to break the paragraphs

rm(list=ls())

## working with the MeetingNotesCopy directory
## to keep titles the same

## load libraries:prelimsetup.R
source('../code/prelimsetup.R')
args = commandArgs(trailingOnly=TRUE)


## Pass in directory locations via the command line
readPath1 <- args[1] ## data
##readPath1 <- "../2018-2019Data/Minutes/"
##outFile <- "01wtoparas.csv"
outFile <- args[2] ## output name



files <- list.files(readPath1, pattern="^[WTCOMTDM]",
                    ignore.case=TRUE)

length(files) ## 69 only?


allparas <- data.frame()
    
for(j in files){
    print(j)
     txt <- read_file(paste0(readPath1, j))
     ## this data I hand split to insert a && at the start of
     ## a paragraph that changed the speaker
    txt <- strsplit(txt, split='&&')

        ## standardize character encoding:
    dat <- as.data.frame(txt,
                         strings.as.factors=FALSE,
                         col.names="text")
    dat$doc <- j 
    ## need to add a paragraph counter
    dat$paranum <- 1:dim(dat)[1]
    dat$key <- paste0(dat$doc, "_para", dat$paranum) ## data row with doc + paranumber
    print("dat made")
    ##print(head(dat))
   
   allparas <- rbind(allparas, dat)
    
}

dim(allparas) ## 5688 X 4

## now figure out the encoding:

print(colnames(allparas)) ## text, doc, paranum, key

allparas$text <- as.character(allparas$text)

class(allparas$text)
Encoding(allparas$text) ## some UTF-8, some "unknown"

table(Encoding(allparas$text)) ## unknown= 4005, UTF-8 1683

## what does one of the "unknown" encodings look like?
head(allparas[5686, "text"])## unknown encoding
head(allparas[5685, "text"]) ## UTF-8 Encoding

## enc2utf8 converts to utf-8:

allparas$text <- enc2utf8(allparas$text)


write.csv(allparas,
          file=outFile)
