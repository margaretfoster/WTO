##script to add metadata to the paragraph-level texts.csv
loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools', 'tidyr', 'quanteda',
           'devtools')

loadPkg(packs)


dataPath <- "~/Dropbox/WTO/data/paras2/"

paras <- read.csv(paste0(dataPath, "parastexts.csv"),
                  header=FALSE, stringsAsFactors=FALSE)

dim(paras) ##6893x2

paras[1:3,]

## stringsplit the path:

library(tidyr)

### dump the path:
paras$V1 <- gsub("/Users/Promachos/Dropbox/WTO/data/paras2/",
                 "", paras$V1 )

## dump .pdf.txt at the end

paras$V1 <- gsub(".pdf.txt",
                 "", paras$V1 )

## paras first row is .DS_store?!. Remove

paras <- paras[-c(1),] ## remove 

class(paras$V1)
 
paras2 <- separate(paras, "V1",
         into=c("para", "parnum", "doc", "docid"),
         sep = "\\.",
         remove = TRUE,
         convert = FALSE)

paras2[1:3,]

paras2$docnum <- as.numeric(paras2$docnum)
paras2$parnum <- as.numeric(paras2$parnum)


## remove unneeded columns

keep <-c("parnum","docid","V2")

paras2 <- paras2[,keep]


paras2[1:3,]

## load in metadata:

##load data

savePath <- "~/Dropbox/WTO/rdatas/"

load(paste0(savePath, 'tradDev20.RData'))
load(paste0(savePath, 'tradeDevMeta.Rdata'))

ls()

head(meta)

meta$files[1:15]

meta$files <- as.character(meta$files)
## split the file names to match doc ID
## and title
##NOTE: COME BACK AND MAKE SURE THE INDEXING IS GOOD!


meta$files <- gsub(".pdf", "", meta$files) 

## match formats
paras2$docid <- as.character(paras2$docid)
paras2$parnum <- as.numeric(paras2$parnum)

### MERGE the paras2 and meta2
## REMEMEBER TO COME BACK AND CHECK INDEX

dim(paras2)
dim(meta)

alldat <- merge(paras2, meta,
                by.x="docid",
                by.y="files",
                all.x=TRUE)


dim(alldat) ## 6892x6

alldat[1:20, 1:6]

write.csv(alldat, file=paste0(savePath,"paragraphsAndMeta.csv"))

