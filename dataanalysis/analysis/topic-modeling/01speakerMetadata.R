### Preliminary analysis of trade and development

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm',
           'tidyr', 'quanteda', "wbstats")

packs2 <- c("stringr", "reshape2",
            "dplyr", "ggplot2")

loadPkg(packs)
loadPkg(packs2)

###########################
#### Declare Paths
##########################

if(Sys.info()['user']=="Ergane"){## desktop
    dataPathDesktop <- "~/Dropbox/WTO/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine
    dataPathDesktop <- "~/Dropbox/WTO/"
    print(paste0("On remote, data path is ", dataPathDesktop))
}


data <- read.csv(paste0(dataPathDesktop,
                        "WTO_TD_NER_Data_July_2020.csv"),
                        stringsAsFactors=FALSE)

colnames(data)

##########################
##Check for gaps in data
##########################

dim(data)
which(is.na(data)) ##61435 61459 61641 61857 63907 65455 67578

nadat <- data[rowSums(is.na(data))>0, ] 

nadat ## 7 rows with meeting numbers, but no text or speakers
data <- data[rowSums(is.na(data))==0,] ## excise

dim(data)


table(data$docid) ## count an spread
length(unique(data$docid)) ##109 meetings

########################
### Reindex paragraph numbers
########################

data <- dplyr::arrange(data, paranum, group_by="docid")## grouping by meeting, not speaker

## meeting number:
data$meetingno <- data$docid
data$meetingno <- as.numeric(gsub(data$meetingno,
                       pattern="WTCOMTDM",
                                  replace=""))

## add meeting-level paragraph counter:
data <- data %>%
    group_by(docid) %>%
    mutate(paranum = dplyr::row_number())

## Add full-data paragraph counter:

## NOTE: the underlying dataframe is still ordered according to
## speaker,so "paranum" counter is sequential within meetings
## but the full-data counter is not.
data$pid <- 1:dim(data)[1] ## paragraph ID, will use for attaching metadata
data$X <- NULL ## get rid of "X" junk row
colnames

########################

############################
## Make date column a "date" class
############################

data$date <- as.Date(data$date) ## format date
data$year <- format(data$date, "%Y") ## add year

summary(data$date) ## 4/4/1995- 6/28/2019


####################################
### Manual DeDuplification 
####################################

## Remove stray whitespace:
data$firstent <- trimws(data$firstent)

## Countries
data[which(data$firstent=="Chairperson"), "firstent"] <- "Chairman"
data[which(data$firstent=="EU"), "firstent"] <- "European Union"
data[which(data$firstent=="EC"), "firstent"] <- "European Communities"
data[which(data$firstent=="UN"), "firstent"] <- "United Nations"
data[which(data$firstent=="Japan, China"), "firstent"] <- "Japan"
data[which(data$firstent=="Chinese Taipei"), "firstent"] <- "Taiwan"
data[which(data$firstent=="Saint Lucia"), "firstent"] <- "St. Lucia"
data[which(data$firstent=="Trinidad"), "firstent"] <- "Trinidad and Tobago"
data[which(data$firstent=="Republic of Korea"), "firstent"] <- "Korea"
## People
data[which(data$firstent=="Dr. Cosgrove-Sacks"), "firstent"] <- "UNECE"
data[which(data$firstent=="El Kabbaj"), "firstent"] <- "IMF/World Bank Development Committee"
data[which(data$firstent=="Lanvin"), "firstent"] <- "UNCTAD"
data[which(data$firstent=="Rossier"), "firstent"] <- "UNCTAD"
data[which(data$firstent=="Deputy Director General"), "firstent"] <- "Deputy Director-General"
data[which(data$firstent=="Ambassador Diallo"), "firstent"] <- "Chairman-Elect"
data[which(data$firstent=="Ambassador Senadhira"), "firstent"] <- "Chairman-Elect"
##Offices and Orgs
data[which(data$firstent=="Sub-committee"), "firstent"] <- "Sub-Committee"
data[which(data$firstent=="UNCTAD Secretariat"), "firstent"] <- "UNCTAD"
data[which(data$firstent=="Technical Cooperation Division"), "firstent"] <- "Technical Cooperation and Training Division"
data[which(data$firstent=="Office for Least-Developed Countries and Africa"), "firstent"] <- "Office for LDCs"
data[which(data$firstent=="NSI"), "firstent"] <- "NS"
data[which(data$firstent=="NEPAD Secretariat"), "firstent"] <- "NEPAD"
data[which(data$firstent=="LDCs"), "firstent"] <- "LDC Group"
data[which(data$firstent=="ITTC Director"), "firstent"] <- "ITTC"
data[which(data$firstent=="Head of Human Resources"), "firstent"] <- "Human Resources Section"

### Merge European Commission, European Communities,
## and European Union together for continutiy:

data[which(data$firstent=="European Communities"), "firstent"] <- "European Union"
data[which(data$firstent=="European Union"), "firstent"] <- "European Union"

table(data$firstent)

############################
#### Create Dataframe with ISO3 Metadata
############################

speakers <- data ## duplicate for failsafe
dim(speakers)## 8666
colnames(speakers)

##########################################
### Bring in World Bank data for countries:
#########################################

library(wbstats)

metacols <- c("country", "region",
              "iso3c","income_level_iso3c" )

meta.inc <- wb_cachelist$countries[,metacols]

head(meta.inc)

#######################
## Use iso3c to create a merge key for the speakers:

key.df <- as.data.frame(table(speakers$firstent))
key.df$Var1 <- as.character(key.df$Var1)

key.df <- merge(key.df,
                meta.inc[,c("country", "iso3c")],
                by.x="Var1",
                by.y="country",
                all.x=TRUE)
                      
######################
## Manually add IS03C for corner cases
######################

key.df[which(key.df$Var1=="Antigua"), "iso3c"] <- "ATG"
key.df[which(key.df$Var1=="Iran"), "iso3c"] <- "IRN"
key.df[which(key.df$Var1=="Chinese Taipei"), "iso3c"] <- "TWN"
key.df[which(key.df$Var1=="Taiwan"), "iso3c"] <- "TWN"
key.df[which(key.df$Var1=="Trinidad"), "iso3c"] <- "TTO"
key.df[which(key.df$Var1=="Venezuela"), "iso3c"] <- "VEN"
key.df[which(key.df$Var1=="Korea"), "iso3c"] <- "KOR"
key.df[which(key.df$Var1=="Cote-d-Ivoire"), "iso3c"] <- "CIV"
key.df[which(key.df$Var1=="Egypt"), "iso3c"] <- "EGY"
key.df[which(key.df$Var1=="Swaziland"), "iso3c"] <- "SWZ"
key.df[which(key.df$Var1=="Hong Kong"), "iso3c"] <- "HKG"

## Fake ISO3 code for admin roles:
key.df[grep("Chairman",x= key.df$Var1), "iso3c"] <- "ADMN"
key.df[grep("Secretariat", x=key.df$Var1), "iso3c"] <- "ADMN"
## Anything with Director, Committee, Division -> Admin
key.df[grep(x=key.df$Var1, "Director"), "iso3c"] <- "ADMN"
key.df[grep(x=key.df$Var1, "[Cc]ommittee"), "iso3c"] <- "ADMN"
key.df[grep(x=key.df$Var1, "Division"), "iso3c"] <- "ADMN"
key.df[grep(x=key.df$Var1, "Section"), "iso3c"] <- "ADMN"

key.df ##Inspect

########################
### Merge in income info
########################

key.df <- merge(key.df,
                meta.inc,
                by="iso3c",
                all.x=TRUE)


head(key.df)
key.df$Freq <- NULL ## don't need
key.df$countries <- NULL ## Var1 superceeds
class(key.df$Var1)

## Faux-iso3c code NOST for non-state actor and admin roles
## Come back to and clean up the non-state entries:

colnames(key.df)

key.df[is.na(key.df$iso3c),metacols] <- "NOTST"
key.df[which(key.df$iso3c=="ADMN"), metacols] <- "NOTST"

key.df[which(key.df$country=="European Union"),
       "income_level_iso3c"] <- "AGG"

tail(key.df)


####### Merge into speakers dataset:

speakers.meta <- merge(x=speakers,
                       y=key.df,
                       by.x="firstent",
                       by.y="Var1",
                       all.x=TRUE)


#################################
## Clean up
################################
                                        
dim(speakers.meta) ## 8659 x15

colnames(speakers.meta)

table(speakers.meta$docid)

which(is.na(speakers.meta)) ## should be none

####

## Add non-state speaker roles
speakers.meta[which(speakers.meta$iso3c=="NOTST"),
              "country"] <- "Nonstate Rep"

#########################
## Read in network summaries
#########################

dp2 <- paste0(dataPathDesktop, "rdatas/")

## Load mcdat2
load(paste0(dp2, "country-meetingRefActivitySums.Rdata"))

colnames(speakers.meta)

head(speakers.meta[,c("firstent", "docid", "income_level_iso3c", "region")])
head(mcdat2)

speakers.meta <- merge(x=speakers.meta,
              y=mcdat2,
              by.x=c("docid", "firstent"),
              by.y=c("docid", "country"),
              all.x=TRUE)

dim(speakers.meta)

ofinterest <- c("docid", "firstent","numrefs",
                "numsends", "delttype", "iso3c")

head(speakers.meta[,ofinterest])

speakers.meta[is.na(speakers.meta$numrefs), 'numrefs'] <- "NOTST"
speakers.meta[is.na(speakers.meta$numsends), 'numsends'] <- "NOTST"
speakers.meta[is.na(speakers.meta$delttype), 'delttype'] <- "NOTST"
speakers.meta[is.na(speakers.meta$delta), 'delta'] <- "NOTST"


head(speakers.meta[,ofinterest])
tail(speakers.meta[,ofinterest])


########################
### Save metadata:
#######################


write.csv(x=speakers.meta, ### write deduplified speaker data:
          file=paste0(dataPathDesktop,
                 "WTO_TD_July2020_DeDup.csv"))

save(speakers.meta,## Save to work with
     file=paste0(dp2,"speakersMeta.Rdata"))
