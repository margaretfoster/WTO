## script to create a subset of data
## for groundtruth benchmarking of NER

dataPath <- "~/Dropbox/WTO/data/"

data <- read.csv(paste0(dataPath, "WTOData.csv"))

dim(data)

set.seed(8008)

n=100
subset <- sample(x=1:dim(data)[1],
                 size=n,
                 replace=FALSE)


tester <- data[subset,]

##write.csv(tester,
##          file=paste0(dataPath, "WTO_NER_GroundTruth.csv"))
##

### Also hand-compare some of the NER results:

dataNER <- read.csv(paste0(dataPath, "NER/NER.csv"))

dim(dataNER)

## take a subset of the NER-identified data:
set.seed(8008)

n=100
subsetNER <- sample(x=1:dim(dataNER)[1],
                    size=n,
                    replace=FALSE)

NERsub <- dataNER[subsetNER,]

NERsub$filename <- as.character(NERsub$filename)


dim(NERsub)


colnames(NERsub)

## match the columns:

library(tidyr)

## separate out filenames:
df <- separate(NERsub,
         col=filename,
         into=c("X", "docid", "parnum"),
         sep="_")

##remove .txt from end of docid:
df$parnum <- sub(x=df$parnum,
                 pattern=".txt",
                 replace="")


dim(df) ## 100x6

head(df)

colnames(data)

head(data[,c("docid", "parnum")])

## add the paragraph text:
df <- merge(df, data[,c("docid",
                        "parnum",
                        "paratext")],
            by=c("docid", "parnum"),
            all.x=TRUE)


dim(df)

colnames(df)

## send to csv for analysis:

write.csv(df,
          file=paste0(dataPath, "NERComparison.csv"))
