## Measure 

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tidyr',
           'quanteda',
           'dplyr',
           'tidyverse',
           "readxl")

##packs.quanteda <- c("quanteda.textmodels",
##                    "quanteda.textplots",
##                    "textreuse", "text2vec")

loadPkg(c(packs))

#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my desktop
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{
    if(Sys.info()['user']=="Promachos"){ ## my laptop
        dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
        print(paste0("The datapath is: ", dataPathDesktop))
    }else{ ## else look in ~/WTO/
        dataPathDesktop <- "../../"
        print(paste0("The datapath is: ", dataPathDesktop))
    }
}

#############################
## Load Data Tagged in Atlas.ti
#############################

## Step 0: create an Atlas.ti document ID-data doc ID key
## Extract pid from Atlas.ti's metadata storage:

data1 <- "~/Desktop/Quotation Manager (3).xlsx"
data2 <- "~/Desktop/WTO Frame Coding 2.xlsx"
rawdata <- read_excel(data1)


df <- rawdata %>%
    separate(ID, c("DocID", "Field"),
             sep=":")

key <-  df %>% filter(Field==3)

key <- key[,c("DocID",
           "Quotation Name",
           "Codes")]

colnames(key) <- c("AtlasDocID", "PID", "Code")

## Step 2: Import the data from Atlas.ti export
## Features: Atlas.ti document ID, binary variables for
## tags & also the etdata field
rawdata2 <- read_excel(data2)


summary(rawdata2) ## want: Document ID;
head(rawdata2) ## Filter away the extraneous
## metadata columns:

colnames(rawdata2)

vars <- c("Maintain CTD",
             "Prioritize Donor Preferences",
             "Prioritize Recepient Preferences",
             "Reciprocator",
             "Redistributor",
             "Reform CTD")

rd2 <- unique(rawdata2[,c("D", vars)]) ## D is id

colnames(rd2) <- c("AtlasID",
                   "Maintain",
                   "DPrefs",
                   "RPrefs",
                   "Recip",
                   "Redist",
                   "Reform")
dim(rd2) ## 453 x 7

head(rd2) ## df structure is one row/ tag; want one row/ obs


rd3 <- rd2 %>% group_by(AtlasID) %>%
    mutate(maintain = sum(Maintain)) %>%
    mutate(dprefs = sum(DPrefs)) %>%
    mutate(rprefs = sum(RPrefs)) %>%
    mutate(recip = sum(Recip)) %>%
    mutate(redist = sum(Redist)) %>%
    mutate(reform = sum(Reform)) 

head(rd3[,c(1, 8:13)])
tail(rd3[,c(1, 8:13)])

summary(rd3) ## all tag variables are binary

rd3[,c(2:7)] <-  NULL

rd3 <- unique(rd3)
dim(rd3) ## 250 x 7

## Step 2: pair key-tags

head(key) ## 250 x3

tags <- merge(key, rd3,
              by.x="AtlasDocID",
              by.y="AtlasID",
              all.y)

dim(tags)
head(tags)


## Need to remove the duplicates:
## Note: should have used a tag to signal them
## Need to filter b/c I didn't put content tags in dups

tags$num.tags <- rowSums(tags[,4:9])

tags[,c(1,2, 10)]

filter.dupes <- c(8197,
                  5285,
                  5117,
                  1595,
                  7969,
                  8325,
                  8097,
                  8308,
                  7225,
                  8291,
                  5632,
                  334,
                  7288)

tags <- subset(tags, !(PID %in% filter.dupes))

dim(tags)## 239 x10

############################
##### Save
############################

write.csv(tags,
          file="parasTaggedFrames.csv")
