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

data$docid <- gsub(pattern=".txt",
                   replace="",
                   x=data$docid)

## Add dates:

meetingdates <- read.csv(paste0("../data/",
                        "TDMeetingDates.csv"),
                 header=TRUE,
                 stringsAsFactors=FALSE)

meetingdates$X <- NULL ## stray row counter

meetingdates$date <- as.Date(meetingdates$date,format="%m/%d/%y")

tail(meetingdates)

## The most recent metings

m1 <- c("WTCOMTDM105", "WTCOMTDM106", "WTCOMTDM107",
        "WTCOMTDM108", "WTCOMTDM109")
d1 <- c("24 APRIL 2018", "5 JULY 2018", "21 NOVEMBER 2018",
        "5 APRIL 2019","28 JUNE 2019")

extrameetings <- as.data.frame(cbind(m1, d1))
colnames(extrameetings) <- c("meeting", "date")

extrameetings$date <- as.Date(extrameetings$date,
                            format="%d %B %Y")

extrameetings

meetingdates <- rbind(meetingdates, extrameetings)

dim(meetingdates) ## 113 x 2

######
data2 <- merge(x=data,
                y=meetingdates,
                by.x="docid",
               by.y="meeting",
               all.x=TRUE)

colnames(data2)

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
