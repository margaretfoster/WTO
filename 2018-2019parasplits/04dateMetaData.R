## open up the data
## This is the first analysis script
## to make sure that the data from the
## WTO minute meetings are reasonably clean and
## speaker-tagged

library(tidyr)
## Declare data:

args = commandArgs(trailingOnly=TRUE)

infile <- args[1] ## input data
outfile <- args[2] ## name of file for ouput

## read in data:

data <- read.csv(infile,
                 stringsAsFactors= FALSE)

data$X <- NULL
data$paranum <- NULL

## fix the lower-case meetings from 25-40

data$docid <- gsub(pattern=".txt",
                   replace="",
                   x=data$docid)

data$docid <- toupper(data$docid)


## Add dates:
meetingdates <- read.csv(paste0("../data/",
                        "TDMeetingDates.csv"),
                 header=TRUE,
                 stringsAsFactors=FALSE)

meetingdates$X <- NULL ## stray row counter

m25to40 <- read.csv(paste0("../data/",
                           "dates25to40.csv"),
                    header=TRUE,
                    stringsAsFactors=FALSE)

## make meeting character match:
m25to40$meeting <- toupper(m25to40$meeting)

head(meetingdates)
head(m25to40)

meetingdates$date <- as.Date(meetingdates$date,format="%m/%d/%y")

m25to40$date <- as.Date(m25to40$date, format="%d %B %Y")

m25to40
tail(meetingdates)

## The most recent metings

m1 <- c("WTCOMTDM105", "WTCOMTDM106", "WTCOMTDM107",
        "WTCOMTDM108", "WTCOMTDM109")
d1 <- c("24 APRIL 2018", "5 JULY 2018", "21 NOVEMBER 2018",
        "5 APRIL 2019","28 JUNE 2019")

m2 <- c("WTCOMTDM19", "WTCOMTDM22")
d2 <- c("6 MARCH 1998", "2 NOVEMBER 1998")

extrameetings <- as.data.frame(cbind(c(m1, m2),
                                      c(d1, d2)))

colnames(extrameetings) <- c("meeting", "date")

extrameetings$date <- as.Date(extrameetings$date,
                            format="%d %B %Y")


meetingdates <- rbind(meetingdates, extrameetings,
                      m25to40[,c("meeting", "date")])

dim(meetingdates) ## 137 x 2

## remove duplicate rows:
meetingdates <- unique(meetingdates)

meetingdates

write.csv(meetingdates,
          file="../data/allmeetingdates.csv")

######

data2 <- merge(x=data,
                y=meetingdates,
                by.x="docid",
               by.y="meeting",
               all.x=TRUE)

colnames(data2)

summary(data2$date) ## 177 NAs


missing <- data2[is.na(data2$date)==TRUE,]


unique(missing$docid) ## "25~"?

## check it out: 
tilde <- data2[grep(data2$docid, pattern="~"),]

dim(tilde) ## 177
unique(tilde$docid) 



## dump the stray tilde:
dim(data2) ##8699

data2 <- data2[data2$docid !="WTCOMTDM25~",]

dim(data2) ##8522

summary(data2$date) ## no NA values

rm(data, meetingdates)

nttexts <- c("para", "docid",
                "key", "firstent",
                "date")


## isolate the paragraph number:
data3 <- tidyr::separate(data2,
                         col="key",
                         into= c("doc", "paranum"),
                         sep = "_para",
                         remove = TRUE)

write.csv(data3,
          file=outfile)

print("done!")
