 
## script breaks the WTO full-text documents into paragraphs
## note though that it will also break "pararaphs" after a page break
## The data looks for [manually-inserted] "&&" which signals a paragraph break

## I manually split the data because the original formatting has been extremely difficult to put together a consistent rule to use to break the paragraphs

rm(list=ls())

## load libraries:prelimsetup.R
source('../code/prelimsetup.R')
args = commandArgs(trailingOnly=TRUE)


## Pass in directory locations via the command line
readPath1 <- args[1] ## data
##readPath1 <- "../2018-2019Data/Minutes/"
##outFile <- "01wtoparas.csv"
outFile <- args[2] ## output name


## Look for files that start with "WTCOMTDM":
files <- list.files(readPath1, pattern="^[WTCOMTDM]",
                    ignore.case=TRUE)

print(paste0("Found", length(files), " minute meetings"))


allparas <- data.frame()
    
for(j in files){
    print(j)
     txt <- read_file(paste0(readPath1, j))
     ## Data has a && inserted at the start of
     ## a paragraph that changed the speaker
    txt <- strsplit(txt, split='&&')

        ## standardize character encoding:
    dat <- as.data.frame(txt,
                         strings.as.factors=FALSE,
                         col.names="text")
    dat$doc <- j 
    ## Add a paragraph counter
    dat$paranum <- 1:dim(dat)[1]
    dat$key <- paste0(dat$doc, "_para", dat$paranum) ## data row with doc + paranumber
    print("dat made")
    ##print(head(dat))
   
   allparas <- rbind(allparas, dat)
    
}


print(paste0("paragraph dimensions:", dim(allparas)))

## now figure out the encoding:

##print(colnames(allparas)) ## text, doc, paranum, key

allparas$text <- as.character(allparas$text)

print("Summary of paragraph encoding schemes in original data:")
table(Encoding(allparas$text)) ##

## enc2utf8 converts to utf-8:
allparas$text <- enc2utf8(allparas$text)

print("Summary of paragraph encoding schemes after conversion:")
table(Encoding(allparas$text)) ##

print("Saving csv" )

write.csv(allparas,
          file=outFile)
