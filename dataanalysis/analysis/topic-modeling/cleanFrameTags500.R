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

loadPkg(c(packs))

#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my desktop
    dataPathDesktop <- "~/Dropbox/WTO-Data/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{
    if(Sys.info()['user']=="Promachos"){ ## my laptop
        dataPathDesktop <- "~/Dropbox/WTO-Data/"
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

data2 <- paste0(dataPathDesktop,"FrameCodingWithPID.xlsx")
rawdata <- read_excel(data2)

## In this format:
## 'Document Name' is the PID

## Step 2: Import the data from Atlas.ti export
## Features: Atlas.ti document ID, binary variables for
## tags & also the etdata field

summary(rawdata) ## want: Document ID;
## Maintain CTD, `Prioritize Donor Preferences,
## Prioritize Recepient Preferences, ""Reciprocator",
## "Redistributor", "Reform CTD'

vars <- c("Document Name", ## PID
          "Maintain CTD",
          "Prioritize Donor Preferences",
          "Prioritize Recepient Preferences",
          "Reciprocator",
          "Redistributor",
          "Reform CTD")

rd2 <- unique(rawdata[,vars])

colnames(rd2) <- c("PID",
                   "Maintain",
                   "DPrefs",
                   "RPrefs",
                   "Recip",
                   "Redist",
                   "Reform")
dim(rd2) ## 839 x 7

## Remove the 13 duplicates:
## (most of which are in the first 250)
## though, I thikn that the way that they get
## aggregated

colnames(rawdata)

dups <- rawdata[which(rawdata$Duplicate==1),]$'Document Name'

rd2 <- subset(rd2, !(PID %in% as.numeric(dups)))

dim(rd2)## 826 x10; note that the 13 removed are all from the
## Top paragraphs set


head(rd2) ## df structure is one row/ tag; want one row/ obs

## This sums for all tags in each docID:
rd3 <- rd2 %>% group_by(PID) %>%
    mutate(maintain = sum(Maintain)) %>%
    mutate(dprefs = sum(DPrefs)) %>%
    mutate(rprefs = sum(RPrefs)) %>%
    mutate(recip = sum(Recip)) %>%
    mutate(redist = sum(Redist)) %>%
    mutate(reform = sum(Reform)) 

colnames(rd3)

head(rd3[,c(1, 8:13)])
tail(rd3[,c(1, 8:13)])

summary(rd3) ## all tag variables are binary

## Remove the original information (not summed by ID)
## This isn't elegant, but oh well
rd3[,colnames(rd2)[2:7]] <-  NULL

rd3 <- unique(rd3)
dim(rd3) ## 487 x 7

############################
##### Save
############################

write.csv(rd3,
          file="parasTaggedFrames500.csv")
