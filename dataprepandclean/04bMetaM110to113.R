## 03/07/20: extract dates from meetings 110-113

library(tidyr)
## Declare data:

infile <- "../03FirstEndM110to113.csv"

## read in data:

data <- read.csv(infile,
                 stringsAsFactors= FALSE)

dim(data)

colnames(data)
head(data[,c(1:3, 5:6)])
head(data$docid)
head(data$key)
head(data$para)

###########################
## Clean up the already-extracted metadata
###########################

data$X <- NULL
data$para <- NULL ## don't know what this is, it's not paranumber

## clean up the paragraph number by removing all before "_para"
data$paranum <- data$key
data$paranum <- gsub(x=data$paranum,
                     pattern = ".*_para",
                     replacement="")
data$paranum <- as.numeric(data$paranum)

head(data$paranum)

## docid
data$docid <- gsub(x=data$docid,
                   pattern=".txt",
                   replacement= "")


## Unique key (which will be different from the keys in the main data)
data$key <- paste0("2020", 1:dim(data)[1])

head(data[,c(1:3, 5:7)])


## Add dates:
meetingdates2020 <- as.data.frame(cbind(c("WTCOMTDM110", "WTCOMTDM111",
                            "WTCOMTDM112", "WTCOMTDM113"),
                          c("22 NOVEMBER 2019", "26 MAY 2020",
                            "29 SEPTEMBER 2020", "20 NOVEMBER 2020")))
colnames(meetingdates2020) <- c("meeting", "date")

head(meetingdates2020)

meetingdates2020$date <- as.Date(meetingdates2020$date, format="%d %B %Y")


######

data2 <- merge(x=data,
                y=meetingdates2020,
                by.x="docid",
               by.y="meeting",
               all.x=TRUE)

colnames(data2)
dim(data2) ##196 x 8 

## Standardize the column names to match the rest of the data:

data2$pid <- data2$key
data2$key <- NULL

## Standardize speaker list to match the rest of the data

data2$firstent <- trimws(data2$firstent)

table(data2$firstent)

data[which(data$firstent=="ITTC Director"), "firstent"] <- "ITTC"
data2[which(data2$firstent=="Chairperson"), "firstent"] <- "Chairman"

## add year column
data2$year <- format(data2$date, "%Y")

##########################################
### Bring in World Bank country metadata:
#########################################


library(wbstats)

metacols <- c("country", "region",
              "iso3c","income_level_iso3c" )

meta.inc <- wb_cachelist$countries[,metacols]

head(meta.inc)

key.df <- as.data.frame(table(data2$firstent))
key.df$Var1 <- as.character(key.df$Var1)

key.df <- merge(key.df,
                meta.inc,
                by.x="Var1",
                by.y="country",
                all.x=TRUE)


key.df ##what are missing:

key.df[grep(x=key.df$Var1, "Gambia"), "iso3c"] <- "GMB"
## Need fake ISO3 code for admin roles:

key.df[grep("Chairman",x= key.df$Var1), "iso3c"] <- "ADMN"
key.df[grep("Committee",x= key.df$Var1), "iso3c"] <- "ADMN"
## One for non-state speakers
key.df[grep("ITTC",x= key.df$Var1), "iso3c"] <- "NOTST"
key.df[grep(x=key.df$Var1, "JAG"), "iso3c"] <- "NOTST"
key.df[grep(x=key.df$Var1, "ESRD"), "iso3c"] <- "NOTST"
key.df[grep(x=key.df$Var1, "ITC"), "iso3c"] <- "NOTST"
key.df[grep(x=key.df$Var1, "WTO"), "iso3c"] <- "NOTST"

key.df[key.df$iso3c=="GMB", "region"] <- "Sub-Saharan Africa"
key.df[key.df$iso3c=="GMB", "income_level_iso3c"] <- "LIC"

#### Code income level for admin and non-state speakers

key.df[key.df$iso3c=="ADMN",
       c("income_level_iso3c", "region")] <- "ADMN"

key.df[key.df$iso3c=="NOTST",
       c("income_level_iso3c", "region")] <-  "NOTST"

key.df[key.df$iso3c=="EUU",
       c("income_level_iso3c")] <-  "AGG"

key.df[key.df$iso3c=="EUU",
       c("region")] <-  "Aggregates"

key.df
### merge into the data:

data <- merge(x=data2,
              y=key.df,
              by.x="firstent",
              by.y="Var1",
              all.x=TRUE)

dim(data)
colnames(data)


head(data[,c(1:2, 4:10)])
tail(data[,c(1:2, 4:10)])

### Add meeting number:

data$meetingno <- as.numeric(gsub(x=data$docid,
                       pattern="WTCOMTDM",
                       replace=""))

table(data$income_level_iso3c)
#####################
## Text cleanup (adapted from 02stmTextClean.R)
####################
#################
library(textclean)

data$cleanedtext <- data$paratext ## make fresh copy

## Ugly unicode:
data$cleanedtext <- textclean::replace_non_ascii(data$cleanedtext,
                                                          replacement=" ",
                                                          remove.nonconverted=TRUE)

## Unicode replacement character becomes "1/4." Dump:

data$cleanedtext <- gsub(data$cleanedtext,
                                  pattern="1\\/4",
                                  replace="")

## Hyphens are also a problem in the final outcome:

data$cleanedtext <- gsub(data$cleanedtext,## retain ecommerce
                                  pattern="e-commerce",
                                  replace="ecommerce")

data$cleanedtext <- gsub(data$cleanedtext,
                                  pattern="-",
                                  replace=" ")

## website words:

data$cleanedtext <- gsub(data$cleanedtext,
                                  pattern="www",
                                  replace="")

data$cleanedtext <- gsub(data$cleanedtext,
                                  pattern="http.",## http or https
                                  replace="")


## Remove extra whitespace
data$cleanedtext <- textclean::replace_white(data$cleanedtext)


head(data$cleanedtext)
tail(data$cleanedtext)

##write.csv(data,
##          file="04Minutes110to113.csv")

##########################
## Merge into the rest of the data
## (via version on Google Drive at 3/07/21)
## NOTE: need to make sure that the date field is
## actually a "date"

masterdat <- read.csv("../WTOCTDParasFeb2021.csv",
                      stringsAsFactors=FALSE)

### Remove all of the network activity variables
### will need to re-derive the adjacency network
### To account for the 115 extra speaker-turns

nonNet <- c( "docid", "firstent", "para", "doc", "paranum", "paratext",
            "ents" , "date", "meetingno", "pid", "year",
            "iso3c" , "country", "region", "income_level_iso3c",
            "cleanedtext")

masterdat <- masterdat[,nonNet]

head(masterdat[,c(1:5, 7:14)])

## vestigial data:
masterdat$doc <- NULL
masterdat$para <- NULL #duplicated with paranum
data$Freq2020 <- NULL
data$Freq <- NULL

colnames(masterdat)
colnames(data)

## convert "date" into as.Date

class(masterdat$date)

masterdat$date[351] ## %Y-m-d

masterdat$date <- as.Date(masterdat$date)

head(masterdat$date)
class(masterdat$date) ## date

## merge the two
### country in the new data:

data$country <- data$firstent
## ID non-state speakers
data[which(data$iso3c=="NOTST"),
              "country"] <- "Nonstate Rep"
data[which(data$iso3c=="ADMN"),
     "country"] <- "Nonstate Rep"

data[,c("firstent", "region", "country")]

setdiff(colnames(masterdat), colnames(data))
setdiff(colnames(data), colnames(masterdat))

masterDat2 <- rbind(masterdat, data)

summary(masterDat2)

write.csv(masterDat2,
          file="WTOSpeakerTurnsM1to113.csv")

print("done!")
