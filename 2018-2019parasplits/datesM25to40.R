## This file to extract dates for
## WTCOMTD Meetings 25-40
## which got lost from the original code
## because for some reaosn they're recorded in lowercase

data <- read.csv("../AllMeetingDates.txt",
         sep=";",
         stringsAsFactors=FALSE,
         header=FALSE)


## strip out unneeded characters:

data$meeting <- gsub(data$V1,
                     pattern="../2018-2019Data/Minutes/",
                     ignore.case = TRUE,
                     replacement="")

data$meeting <- gsub(data$meeting,
                     pattern=".txt",
                     replacement= "")


data$meeting <- gsub(data$meeting,
                     pattern="/",
                     replacement= "")
data$meeting <- as.character(data$meeting)

data$meeting <- trimws(data$meeting, which = c("both"))


data$date <- gsub(data$V2,
                  replacement="",
                  pattern="note on the meeting of",
                  ignore.case=TRUE)

## only need minutes 25-40
## (and don't want to clean the rest)

minutes.want <- paste0("wtcomtdM", 25:40)

minutes.want <- c(minutes.want, "wtcomtdM28A1")

minutes.want

as.character(data$meeting)


subset <- data[as.character(data$meeting) %in% minutes.want,]


dim(subset)  ## 16x4


subset[which(subset$meeting=="wtcomtdM29"),
             c("date")] <- "28 JUNE 2000"

subset[which(subset$meeting=="wtcomtdM31"),
             c("date")] <- "27 OCTOBER 2000"


subset[which(subset$meeting=="wtcomtdM34"),
             c("date")] <- "22 MAY 2001"


subset[,c("meeting", "date")]

write.csv(subset[,c("meeting", "date")],
          file="dates25to40.csv")
