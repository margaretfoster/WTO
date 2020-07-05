### Preliminary analysis of trade and development

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

packs2 <- c("stringr", "reshape2",
            "dplyr", "ggplot2",  "magrittr")

loadPkg(packs)
loadPkg(packs2)

dataPath <- "../../"
savePath <- "./"


data <- read.csv(paste0(dataPath,
                        "WTO_TD_NER_Data_July_2020.csv"),
                        stringsAsFactors=FALSE)

colnames(data)

data$X <- NULL
data$date <- as.Date(data$date)

class(data$date)
hist(data$date,breaks="months")
summary(data$date) ## 4/4/1995- 6/28/2019

##want to add a column for year:
data$year <- format(data$date, "%Y")

colnames(data)
dim(data) ## 8636

## Remove data for "meeting" 28A1, which is an appendix with a
## program of a 2000 seminar on differential treatment of economies

data <- data[!data$docid=="WTCOMTDM28A1",]

dim(data) ##8636x10

## Consolidate the "EU" and "EC" entries with
## European Union and European Communities respectively
## same with UN and United Nations
data[which(data$firstent=="EU"), "firstent"] <- "European Union"
data[which(data$firstent=="EC"), "firstent"] <- "European Communities"
data[which(data$firstent=="UN"), "firstent"] <- "United Nations"
data[which(data$firstent=="Japan, China"), "firstent"] <- "Japan"
data[which(data$firstent=="Chinese Taipei"), "firstent"] <- "Taiwan"
data[which(data$firstent=="Dr. Cosgrove-Sacks"), "firstent"] <- "UNECE"

#### Summarize frequency and variety of speakers

data$meetingno <- data$docid
data$meetingno <- as.numeric(gsub(data$meetingno,
                       pattern="WTCOMTDM",
                       replace=""))


speakers <- data

dim(speakers)## 8636
head(speakers)

### Bring in World Bank data for countries:
library(wbstats)

str(wb_cachelist, max.level = 1)## what variables are there?

class(wb_cachelist) ## list

head(wb_cachelist$countries) ## has "income" as a variable:

meta.inc <- wb_cachelist$countries[,c("country", "region",
                                      "iso3c", "incomeID",
                                      "income")]

## Test to see what might be missing in merge:

tmp1 <- unique(speakers$firstent)
length(tmp1) ## 155

overlap <- intersect(tmp1, meta.inc$country)

missing <- setdiff(tmp1, meta.inc$country) ## countries in WTO speakers list but not WB metadata dataaset

length(overlap) ## 110.

length(missing) ## needed to hand-add: Iran, Antigua, Chainese Taipei;
## Trinidad; Hong Kong, [south] Korea; Egypt; Venezuela

## need to grep for the official names for those countries
## us iso3c to create a merge key for the speakers:

## 1- merge iso3c ids into my WTO data; then hand-add those 8 
## then use iso3 to merge in the rest of the WB data

## want: dataframe with my speaker countries + iso3c codes

key.df <- as.data.frame(table(speakers$firstent))

dim(key.df) ## 154 x2

key.df$Var1 <- as.character(key.df$Var1)

head(key.df)


class(key.df$Var1)

key.df <- merge(key.df,
                meta.inc[,c("country", "iso3c")],
                by.x="Var1",
                by.y="country",
                all.x=TRUE)
                        

head(key.df)

key.df[which(key.df$Var1=="Antigua"), "iso3c"] <- "ATG"
key.df[which(key.df$Var1=="Iran"), "iso3c"] <- "IRN"
key.df[which(key.df$Var1=="Chinese Taipei"), "iso3c"] <- "TWN"
key.df[which(key.df$Var1=="Trinidad"), "iso3c"] <- "TTO"
key.df[which(key.df$Var1=="Venezuela"), "iso3c"] <- "VEN"
key.df[which(key.df$Var1=="Korea"), "iso3c"] <- "KOR"
key.df[which(key.df$Var1=="Cote-d-Ivoire"), "iso3c"] <- "CIV"
key.df[which(key.df$Var1=="Egypt"), "iso3c"] <- "EGY"
## Now that I have the iso3 codes for the countires in my data
## merge in the rest of the meta.inc information into key.df

head(key.df)

key.df <- merge(key.df,
                meta.inc,
                by="iso3c",
                all.x=TRUE)


head(key.df)
key.df$Freq <- NULL ## don't need
key.df$countries <- NULL ## Var1 superceeds
class(key.df$Var1)

## give a faux-iso3c code NOST for non-state actor
## call "Chairman", "Committee", Secretariat as ADMN
## now pull this data into the speakers dataset:

## Come back to and clean up the non-state entries:

key.df[which(key.df$Var1=="Chairman"), "iso3c"] <- "ADMN"
key.df[which(key.df$Var1=="Chairperson"), "iso3c"] <- "ADMN"
key.df[which(key.df$Var1=="Secretariat"), "iso3c"] <- "ADMN"
key.df[which(key.df$Var1=="Committee"), "iso3c"] <- "ADMN"
key.df[which(key.df$iso3c==NA), "iso3c"] <- "NOST"

key.df[is.na(key.df$iso3c),c("iso3c", "region",
                             "incomeID", "income")] <- "NOTST"

key.df[which(key.df$iso3c=="ADMN"),c("region",
         "incomeID", "income")] <- "NOTST"

key.df[which(key.df$country=="European Union"), "incomeID"] <- "AGG"

####### Merge into speakers dataset:

speakers.meta <- merge(x=speakers,
                       y=key.df,
                       by.x="firstent",
                       by.y="Var1",
                       all.x=TRUE)

dim(speakers)
dim(speakers.meta) ## 8636x 9, did work, just has a problem with the
## entries with no identified spekers


#########
## Clean up
###########
                                        
dim(speakers.meta) ## 8636, so need to find the 7 that don't have anything

speakers.meta$incomeID <- as.factor(speakers.meta$incomeID)

## There seems to be 7 rows that have document IDs and paragraph numbers,
## but no text, or speakers. Will drop those:

speakers.meta <- speakers.meta[!is.na(speakers.meta$incomeID==TRUE),]

## Conlidate some technically distinct, but theoretically linked
## unique speakers:

## Consolidate European Union, European Communities, European Commission
## not technically the same, of course, but.. similar actor:
speakers.meta$firstent <- as.character(speakers.meta$firstent)

## "European Commission"  "European Communities" "European Union
## choosing European Communities as the base because it is the most common
speakers.meta[which(speakers.meta$firstent=="European Commission"),
              "firstent"] <- "European Communities"

speakers.meta[which(speakers.meta$firstent=="European Union"),
                       "firstent"] <- "European Communities"

### Now consolidate Chairperson into Chairman:

speakers.meta[which(speakers.meta$firstent=="Chairperson"),
              "firstent"] <- "Chairman"

############################################
### Fill in some holes in the data:
###########################################

#### Find the missing date or document id:

which(is.na(speakers.meta$date))## 604

## paragraph in meeting 76 from Burkina Faso:
## add date for meeting 76

speakers.meta[604, "date"] <- "2009-10-12"
speakers.meta[604, "year"] <- 2009


### Add a "country" name for Admin roles so that we don't get
## lots of NAs:

## Add the admin roles:

speakers.meta[which(speakers.meta$iso3c=="ADMN"),
              "country"] <- "Administration"


## Add non-state speaker roles
speakers.meta[which(speakers.meta$iso3c=="NOTST"),
              "country"] <- "Nonstate Rep"


########################
### Save metadata:
#######################

save(speakers.meta,
     file="speakersMeta.Rdata")
