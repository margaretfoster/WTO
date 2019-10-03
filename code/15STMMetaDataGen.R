## This script to generate the
## data needed for a structural topic model
## with covariates:
## US Administration
## Trade Rep?
## (and new representative)


rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

paradata <- read.csv("~/Dropbox/WTO/data/NER/WTODataNew.csv",
                     header=FALSE, stringsAsFactors=FALSE)


dim(paradata) ## 12811

## Make meaningful column names:

## V1 = file name
## V2 = doc ID
## V3 = para number
## V4 = para text
## V5 = speaker
## V6 = date
## V8= in-paragraph reference

colnames <- c("file", "meeting", "paragraph",
              "paratext", "speaker", "date",
              "docrefid", "refs")

colnames(paradata) <- colnames

paradata$date <- as.Date(paradata$date, format="%m/%d/%y")
paradata$year <- format(as.Date(paradata$date,
                               format="%d/%m/%Y"),"%Y")
paradata$month <- format(as.Date(paradata$date,
                                 format="%d/%m/%Y"),"%m")


paradata$year <- as.numeric(paradata$year)
paradata$month <- as.numeric(paradata$month)

## subset: who speaks in what order:
speakers <- paradata[,c("meeting",'paragraph', 'speaker', 'date')]


## goal: sort by date, then paragraph number,
## then add a counter that restarts at year breaks

speakers <- paradata[,c("meeting",'paragraph',
                        'speaker', 'date',
                        'year', 'month')]

spk <- data.frame()
for(m in unique(speakers$meeting)){
    dt <- speakers[which(speakers$meeting==m),]
    dt <- dt[order(dt$date, dt$paragraph),]
    dt$counter <- 1:dim(dt)[1]
    spk <- rbind(spk, dt)
}

### Add metadata to "paradata" dataframe:

colnames(paradata)


head(paradata[,7:10])

## date ranges:
summary(paradata$year) ## 1995-2018

###################
## Add metadata:
## US administration changes
## Want:

## Administration 
## Transitional Period (November-January)
## New Admin Takeover (January)

## Admins:
## Clinton:  1993-January 20, 2001
## Transitional (Clinton -> Bush) November 2000 - January 2001
## Bush: Feburary 2001- January 2009
## Transitional (Bush-> Obama) November 2008 - January 2009
## Obama: Febrary 2009 -> January 2017
## Transitional (Obama -> Trump) November 2016 - January 2017

Clinton <- c(1993:2000)
Bush <- c(2001:2008) ## need to go in and remove Jan
Obama <- c(2009:2016)
Trump <- c(2017:2018)

## dummy for being in clinton years:
paradata$clinton <- ifelse(paradata$year %in% Clinton, 1, 0)
paradata$bush <- ifelse(paradata$year %in% Bush, 1, 0)
paradata$obama <- ifelse(paradata$year %in% Obama, 1, 0)
paradata$trump <- ifelse(paradata$year %in% Trump, 1,0)

paradata$id <- 1:dim(paradata)[1]

## transitional clinton-Bush
## month == (11 or 12) and year==2000 or month = 1 and year 2001

## Trying to get following combination: 11/2000, 12/2000, 1/2001
## Clinton-Bush Transition:
cbtrans <- paradata[((paradata$month==11 |
                   paradata$month==12) &
                  paradata$year==2000) |
                 (paradata$month==1 &
                  paradata$year==2001), "id"]


dim(cbtrans) ## None. Confirmed nothing on website

## Bush-Obama

botrans <-paradata[((paradata$month==11 |
                   paradata$month==12) &
                  paradata$year==2008) |
                 (paradata$month==1 &
                  paradata$year==2009), "id"]


length(botrans) ## 99 paragraphs

paradata$bushobmtrans <- ifelse(paradata$id %in% botrans, 1, 0)

## Obama-Trump
ottrans <- paradata[((paradata$month==11 |
                   paradata$month==12) &
                  paradata$year==2016) |
                 (paradata$month==1 &
                  paradata$year==2017), "id"]

length(ottrans) ##431

paradata$obtrumptrans <- ifelse(paradata$id %in% ottrans, 1, 0)

summary(paradata$obtrumptrans)

## clean up the January entries:

## Coded as both Obama and an Obama-Trump transition
paradata$obama[which(paradata$obama==1 &
                     paradata$obtrumptrans == 1)] <- 0


## coded as both Bush and Bush-Obama transition
paradata$bush[which(paradata$bush==1 &
                     paradata$bushobmtrans == 1)] <- 0


## Refers to United States:

paradata$reftoUS <-0 ## new variable

paradata[grep(pattern="United States",
              paradata$refs), ]$reftoUS <- 1

table(paradata$reftoUS) ## 833 = yes; 12978 no

## ref to China:

paradata$reftoCh <- 0

paradata[grep(pattern="China",
              paradata$refs), ]$reftoCh <- 1

table(paradata$reftoCh) ## 13103 vs 708


## Paragraphs that refer to one of the most frequent
## in-reference nodes
## In asending order: China, Canada, Egypt, India, US:

paradata$refbig5 <- "other"

paradata[grep(pattern="United States",
              paradata$refs), ]$refbig5 <- "United States"

paradata[grep(pattern="India",
              paradata$refs), ]$refbig5 <- "India"

paradata[grep(pattern="Egypt",
              paradata$refs), ]$refbig5 <- "Egypt"

paradata[grep(pattern="Canada",
              paradata$refs), ]$refbig5 <- "Canada"

paradata[grep(pattern="China",
              paradata$refs), ]$refbig5 <- "China"
######################

## Send out:
#######################

save(paradata,
      file="WTOParagraphDataForSTM.Rdata")
