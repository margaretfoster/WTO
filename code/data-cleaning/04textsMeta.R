
##script to add metadata to the paragraph-level texts.csv
## this is designed for working with texts.csv and
## no master list of automatically extracted metadata


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
        { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools', 'tidyr', 'quanteda',
           'devtools')

loadPkg(packs)

dataPath <- "../../paras/"

metadat <- "../metadata/"

paras <- read.csv(paste0("texts.csv"),
                  header=FALSE, stringsAsFactors=FALSE)

dim(paras) ##13812x2

paras[1:3,]

## stringsplit the path:

library(tidyr)

### dump the path:

abspath <- "/home/mjf34/WTO/paras/" ## for VM

paras$V1 <- gsub(abspath,
                 "", paras$V1 )

## dump .pdf.txt at the end

paras$V1 <- gsub(".txt",
                 "", paras$V1 )

##################################
### Read in Metadata Information
###################################

### Bring in speaker information
### (Indexed to paragraph)

speakers <- read.csv(paste0(metadat,
                            "speakers.csv"),
                     header=FALSE)

dim(speakers) ##13812 x1

speakers$V1 <- as.character(speakers$V1)

paraspath <- "../../paras//"

speakers$V1 <- gsub(paraspath, "",
                speakers$V1)


## split out the speaker country:

speakers <- separate(speakers,
                     col="V1",
                     into=c("para","speaker"),
                     sep= ".txt")

speakers[1:50,]
## Rows with extra content converted into NA
## remove those:



speakers$para <- as.character(speakers$para)
speakers$speaker <- as.character(speakers$speaker)
paras$V1 <- as.character(paras$V1)
speakers[1250: 1260,]


####################
## merge in speakers:
###################


## Merge the speaker and text:
## ensure that sets in speakers paragraphs
## and paras paragraphs are the same:

which(!(paras$V1 %in% speakers$para)) ## complete overlap
which(!(speakers$para %in% paras$V1)) ## none missing

paras <- merge(paras, speakers,
               by.x="V1", by.y="para",
               all.x=TRUE, all.y=TRUE)

if(dim(paras)[1] != dim(speakers)[1]){
    print("alert! unusual number of rows")}

paras[155:160, ]

##################
## Now merge in dates
##################
## Dates at the document, not paragraph level:

## Clean up paragraph and meeting information

paras2 <- separate(paras, "V1",
         into=c("para", "parnum", "doc", "docid"),
         sep = "\\.",
         remove = TRUE,
         convert = FALSE)

paras2[910:915,]

paras2$speaker

## which are "fake" empty:
tst <- paras2[which(paras2$speaker==" "), c("docid", "speaker")]

##make those into NA:
paras2$speaker[which(paras2$speaker==" ")] <- NA


paras2$docnum <- as.character(paras2$docid)
paras2$parnum <- as.numeric(paras2$parnum)

## Now merge in date information

dates <- read.csv(paste0(metadat,"datemetadata.csv"),
                  sep=";",
                  header=FALSE)

dim(dates) ##108 x2

colnames(dates)

dates$V1 <- as.character(dates$V1)
dates$V2 <- as.character(dates$V2)


dates[67:72,]
dates[45:56,]

## remove unneeded content:

filepath <- "../../meetingnotes/txtextracts/"
preamble <- "NOTE ON THE MEETING OF "


dates$V1 <- gsub(filepath, "", dates$V1)
dates$V2 <- gsub(preamble, "", dates$V2)
dates$V1 <- gsub(".txt", "", dates$V1)

dates[1:15,]

## pull out the ones with multiple dates, by looking for commas and "AND"
## actually do these by hand. Ugly, but uglier not to:

dates$extradate <- NA

## Make sure you need these and the indexing lines up:
dates[which(dates$V2==" 24 AND 25 NOVEMBER 2016"), c("V2", "extradate")] <- c("24 NOVEMBER 2016", "25 NOVEMBER 2016")
dates[which(dates$V2==" 27 JANUARY AND 14 MARCH 20171"),
      c("V2", "extradate")] <- c("27 JANUARY 2017", "14 MARCH 2017")
dates[which(dates$V2==" 15 AND 31 OCTOBER 1996"),
      c("V2", "extradate")] <- c("15 OCTOBER 1996", "31 OCTOBER 1996")
dates[which(dates$V2== " 7-8 JULY 1999NOTE BY THE SECRETARIAT"),
      c("V2", "extradate")] <- c("7 JULY 1999", "8 JULY 1999")
dates[which(dates$V2==" 28 JUNE AND 10 JULY 2000"),
      c("V2", "extradate")] <- c("7 JUNE 2000", "10 JULY 2000")
dates[which(dates$V2==" 27 OCTOBER AND 8 NOVEMBER 2000"),
      c("V2", "extradate")] <- c("27 OCTOBER 2000", "8 NOVEMBER 2000")
dates[which(dates$V2==" 8 AND 9 OCTOBER 2002"),
      c("V2", "extradate")] <- c("8 OCTOBER 2002", "9 OCTOBER 2002")
dates[which(dates$V2==" 11, 18, 19 AND 22 NOVEMBER 2002"),
      c("V2", "extradate")] <- c("11 NOVEMBER 2002", "18, 19, 22 NOVEMBER 2002")
dates[which(dates$V2==" 22 MAY AND 12 JUNE 2003"),
      c("V2", "extradate")] <- c("22 MAY 2003", "12 JUNE 2003")
dates[which(dates$V2==" 16 AND 23 OCTOBER 2003"),
      c("V2", "extradate")] <- c("16 OCTOBER 2003", "23 OCTOBER 2003")
dates[which(dates$V2==" 27 AND 28 NOVEMBER 2003"),
      c("V2", "extradate")] <- c("27 NOVEMBER 2003", "28 NOVEMBER 2003")
dates[which(dates$V2==" 28 AND 29 SEPTEMBER 2004"),
      c("V2", "extradate")] <- c("28 SEPTEMBER 2004", "29 SEPTEMBER 2004")
dates[which(dates$V2==" 16, 26 NOVEMBER AND 10 DECEMBER 2004"),
      c("V2", "extradate")] <- c("16 NOVEMBER 2004", "26 NOVEMBER AND 10 DECEMBER 2004")
dates[which(dates$V2==" NOTE ON THE MEETINGS OF 24 MAY AND 7 JUNE 1996"),
      c("V2", "extradate")] <- c("24 MAY 1996", "7 JUNE 1996")
dates[which(dates$V2==" NOTE ON THE MEETINGS OF 24 MAY AND 7 JUNE 1996"),
      c("V2", "extradate")] <- c("24 MAY 1996", "7 JUNE 1996")
dates[which(dates$V2==" 12 OCTOBER AND 3 NOVEMBER1 2009"),
      c("V2", "extradate")] <- c("12 OCTOBER 2009", "3 NOVEMBER 2009")
dates[which(dates$V2==" 21 JUNE AND 5 JULY 20111"),
      c("V2", "extradate")] <- c("21 JUNE 2011", "5 JULY 2011")
dates[which(dates$V2==" 14, 17, 18, 25 AND 28 NOVEMBER 20111"),
            c("V2", "extradate")] <- c("14 NOVEMBER 2011", "17, 18, 25, 28 NOVEMBER 2011")

dates

## get rid of extra whitespace:

dates$V1 <- gsub(" ", "", dates$V1)
dates$V2 <- gsub(" ", "", dates$V2)

### break paras into paragraph ID, doc ID:

# first pass about whether this will throw a problem:


which(!(paras2$docid %in% dates$V1))
which(!(dates$V1 %in% paras2$docid))


colnames(paras2)

paras2[1:5,]

df3 <- merge(paras2, dates,
             by.x="docid", by.y="V1",
             all.x=TRUE)

dim(df3)


df3[1060: 1065,]
## Drop extraneous columns:

df3$para <- NULL
df3$doc <- NULL
df3$docnum <-  NULL

## Formate primary date:

df3$V2.y <- as.Date(df3$V2.y,
                    format="%d%B%Y")

colnames(df3)

colnames(df3) <- c("docid", "parnum", "paratext",
                   "country.speaker", "date", "extradates")

df3[1:5,]


wto1 <- df3

############################
##fill in missing speakers:
############################

wto1 <- wto1[order(as.factor(wto1$docid), wto1$parnum),]

wto1[1:200, c("docid", "parnum", "country.speaker")]

## create a "blank" speaker for content from para 1 until
## first Chairman entry:
## first ensure no actual speakers in the first paras:

wto1[which(wto1$parnum==1),c("docid", "country.speaker")]

## now put a 'no speaker' in:

wto1[which(wto1$parnum==1), "country.speaker"] <- "No Speaker"

## now use tidyR fill()
library(tidyr)

wto1 <- fill(wto1, country.speaker,
             .direction=c("down"))

wto1[1500:1700,c("docid", "parnum", "country.speaker")]

wto1[1:200,c("docid", "parnum", "country.speaker") ]

## spot check some of the filled-in sections against the ground-truth docs
## WTCOMTDM21     51 & 52, 68-71 should be  European Communities
## WTCOMTDM15      9& 10  should be  Morocco
## 1163 WTCOMTDM15      13-15 should be chairman

## texts should refer to the European Communities rep:
wto1[which(wto1$docid=="WTCOMTDM21" &
           wto1$parnum==68),]

wto1[which(wto1$docid=="WTCOMTDM21" &
           wto1$parnum==69),]

wto1[which(wto1$docid=="WTCOMTDM21" &wto1$parnum==70),]

## texts should relate to Morocco

wto1[which(wto1$docid=="WTCOMTDM15" &
           wto1$parnum==9),]

wto1[which(wto1$docid=="WTCOMTDM15" &
           wto1$parnum==10),]

wto1[which(wto1$docid=="WTCOMTDM15" &
           wto1$parnum==9),]

wto1[which(wto1$docid=="WTCOMTDM15" &
           wto1$parnum==10),]

## should be chairman:

wto1[which(wto1$docid=="WTCOMTDM15" &
           wto1$parnum==13),]

wto1[which(wto1$docid=="WTCOMTDM15" &
           wto1$parnum==14),]


###############################3
## Clean up some of the country.speakers
################################


## Take out some recurring mess:
scrap <- c("Ambassador", "Members", "Sir",
           "Mr.", "Dr.")

for(i in scrap){
    wto1$country.speaker <- gsub(i, "", wto1$country.speaker)
}

## trim leading and trailing whitespace:

wto1$country.speaker <- trimws(wto1$country.speaker,
                               which=c("both"))


## Note that "The delegate of" notation only shows up in
## notes from meeting 50

freqSpeakers <- as.data.frame(table(wto1$country.speaker))

freqSpeakers


## Now case-by-case cleanups:


## "Institute"
wto1[grep("Institute", wto1$country.speaker),]

wto1[grep("Institute", wto1$country.speaker),
     "country.speaker"] <- "ITTC"

## Budget
wto1[grep("Budget", wto1$country.speaker),]

wto1[grep("Budget", wto1$country.speaker),
     "country.speaker"] <-  "Budget and Control Section"

## Food
wto1[grep("Food", wto1$country.speaker),] ## food and agricultural organization = FAO

wto1[grep("Food", wto1$country.speaker),
     "country.speaker"] <-  "FAO"

## Mystery European
## some are European Union some are European Communities

wto1[which(wto1$country.speaker=="European"),]
## Côte

wto1[grep("Côte", wto1$country.speaker),] 
wto1[grep("Cote", wto1$country.speaker),]

wto1[grep("Côte", wto1$country.speaker),
     "country.speaker"] <-  "Côte d'Ivoire"

wto1[grep("Cote", wto1$country.speaker),
     "country.speaker"] <-  "Côte d'Ivoire"

## India Members


## Arab Maghreb

wto1[grep("Arab Maghreb", wto1$country.speaker),]

wto1[grep("Arab Maghreb", wto1$country.speaker),
     "country.speaker"] <-  "Arab Maghreb Union"

## Common Fund

wto1[grep("Common Fund", wto1$country.speaker),]

wto1[grep("Common Fund", wto1$country.speaker),
     "country.speaker"] <-  "Common Fund For Commodities"

## Republic Korea
 
wto1[grep("Republic Korea", wto1$country.speaker),
     "country.speaker"] <-  "Republic of Korea"

## Some stray second countries:
## Brazil

wto1[grep("Brazil *", wto1$country.speaker),
     "country.speaker"] <-  "Brazil"

## Guatemala

wto1[grep("Guatemala *", wto1$country.speaker),
     "country.speaker"] <-  "Guatemala"
## Rwanda

wto1[grep("Rwanda *", wto1$country.speaker),]

wto1[grep("Rwanda *", wto1$country.speaker),
     "country.speaker"] <-  "Rwanda"

##
freqSpeakers <- as.data.frame(table(wto1$country.speaker))
freqSpeakers

### More cleaning:

wto1[grep("Venezuela *", wto1$country.speaker),]

wto1[grep("Venezuela *", wto1$country.speaker),
     "country.speaker"] <-  "Venezuela"

wto1[grep("St Lucia *", wto1$country.speaker),
     "country.speaker"] <-  "St. Lucia"

wto1[grep("Mauritius *", wto1$country.speaker),
     "country.speaker"] <-  "Mauritius"

wto1[grep("Hong Kong China", wto1$country.speaker),]


wto1[grep("Hong Kong *", wto1$country.speaker),
     "country.speaker"] <-  "Hong Kong"

wto1[grep("Panama *", wto1$country.speaker),
     "country.speaker"] <-  "Panama"

wto1[grep("United Arab*", wto1$country.speaker),
     "country.speaker"] <-  "United Arab Emirates"

wto1[grep("Guinea *", wto1$country.speaker),
     "country.speaker"] <-  "Guinea"

wto1[grep("Egypt *", wto1$country.speaker),
     "country.speaker"] <-  "Egypt"
###

wto1[grep("Strategic Planning", wto1$country.speaker),
     "country.speaker"] <- "Strategic Planning, Monitoring and Evaluation Section"

##
wto1[grep("India *", wto1$country.speaker),
     "country.speaker"] <- "India"  

wto1[grep("Economic", wto1$country.speaker),
     "country.speaker"] <- "Economic Research and Statistics Division"

wto1[grep("Unites States", wto1$country.speaker),
     "country.speaker"] <-  "United States"

## these are all the us:
wto1[which(wto1$country.speaker=="United"),
     "country.speaker"] <-  "United States"

wto1[grep("West African", wto1$country.speaker),]

wto1[grep("West African", wto1$country.speaker),
     "country.speaker"] <-  "West African Economic and Monetary Union"


## Some spacing issues:

wto1[grep("Mexico *", wto1$country.speaker),
     "country.speaker"] <-  "Mexico"

wto1[grep("Japan *", wto1$country.speaker),
     "country.speaker"] <-  "Japan"

###
wto1[grep("Sweden *", wto1$country.speaker),
     "country.speaker"] <-  "Sweden"

wto1[grep("Cuba *", wto1$country.speaker),
     "country.speaker"] <-  "Cuba"


wto1[grep("Argentina *", wto1$country.speaker),
     "country.speaker"] <-  "Argentina"


wto1[grep("Australia *", wto1$country.speaker),
     "country.speaker"] <-  "Australia"

wto1[grep("Benin *", wto1$country.speaker),
     "country.speaker"] <-  "Benin"


wto1[grep("Nigeria *", wto1$country.speaker),
     "country.speaker"] <-  "Nigeria"

wto1[grep("Jordan *", wto1$country.speaker),
     "country.speaker"] <-  "Jordan"

wto1[grep("Switzerland *", wto1$country.speaker),
     "country.speaker"] <-  "Switzerland"

wto1[grep("Tunisia *", wto1$country.speaker),
     "country.speaker"] <-  "Tunisia"

wto1[grep("Cost Rica", wto1$country.speaker),
     "country.speaker"] <-  "Costa Rica"

wto1[grep("Antigua *", wto1$country.speaker),
     "country.speaker"] <-  "Antigua"

wto1[grep("China *", wto1$country.speaker),
     "country.speaker"] <-  "China"

## Standardize some spelling

wto1[grep("St Lucia", wto1$country.speaker),
     "country.speaker"] <-  "St. Lucia"

wto1[grep("St. Lucia", wto1$country.speaker),
     "country.speaker"] <-  "Saint Lucia"


wto1[grep("Bahr*", wto1$country.speaker),
     "country.speaker"] <-  "Bahrain"

wto1[grep("Bolivarian R*", wto1$country.speaker),
     "country.speaker"] <-  "Venezuela"

wto1[grep("Central African *", wto1$country.speaker),
     "country.speaker"] <-  "Central African Republic"


wto1[grep("European Community", wto1$country.speaker),
     "country.speaker"] <-  "European Communities"


wto1[grep("Kenya *", wto1$country.speaker),
     "country.speaker"] <-  "Kenya"

wto1[grep("Maritius *", wto1$country.speaker),
     "country.speaker"] <-  "Maritius"

wto1[grep("Malta *", wto1$country.speaker),
     "country.speaker"] <-  "Malta"

## ICCO and ICO same?

## ICO = international coffee organization
wto1[grep("ICO", wto1$country.speaker),]

## ICCO = International Cocoa Organization 
wto1[grep("ICCO", wto1$country.speaker),]


## "Kingdom" alone is Saudi

wto1[which(wto1$country.speaker=="Kingdom"),
    "country.speaker"] <-  "Saudi Arabia"

## UNCTAD Secretariat and UNCTAD are different reps
wto1[grep("UNCTAD S*", wto1$country.speaker),]

## "EC" is in the data that way
wto1[grep("EC", wto1$country.speaker),]

## "European": mix of European Union and European Communities

unique(wto1[which(wto1$country.speaker=="European"),]$date)

## 1/27/17 = EU
## 2016-07-08 = EU
## 2014-11-27 = EU
## 2017-06-21 = EU

## 2001-10-08 = European Communities 
wto1[wto1$country.speaker=="European" &
     ((as.character(wto1$date)=="2017-06-21")|
     (as.character(wto1$date)=="2014-11-27")|
     (as.character(wto1$date)=="2016-07-08")|
     (as.character(wto1$date)=="2017-01-27")),
     "country.speaker"] <-  "European Union"

wto1[wto1$country.speaker=="European" &
     (as.character(wto1$date)=="2001-10-08"),
         "country.speaker"] <- "European Communities"


##Which are "republic"
## all Republic of Korea
wto1[which(wto1$country.speaker=="Republic"),
     "country.speaker"] <-  "Korea"

## standardize the Korea entries:

wto1[which(wto1$country.speaker=="Korea"),
     "country.speaker"] <-  "Republic of Korea"

wto1[which(wto1$country.speaker=="Organization"),
     "country.speaker"] <-"Organization of African Unity and Economic Community" 
###

freqSpeakers <- as.data.frame(table(wto1$country.speaker))
freqSpeakers

## Get rid of the "\n" markup"

wto1$paratext <- gsub(pattern="\n",
                      replacement= " ",
                      x= wto1$paratext)


## get rid of extra whitespace
## below converts more than two white spaces in a row to one
## white space

wto1$paratext <- gsub(" {2,}"," ",wto1$paratext)

## spot check:

rand <- sample(1:dim(wto1)[1], 10, replace=F)

wto1$paratext[rand]


## Some missing dates:

##Missing data?                                                                                                                                               

sapply(wto1, function(x) sum(is.na(x)))

## remove rows with missing date:                                                                                                                             

wto1[which(is.na(wto1$date)), "docid"] ## all com 34                                                                                                          
## which is May 22, 2001  or WTCOMTDM14R1 which is Feb 17, 1997                                                                                               

class(wto1$docid)

which(wto1$docid=="WTCOMTDM14R1")

wto1[which(wto1$docid=="WTCOMTDM14R1"),
     "date"] <- as.Date("17February1997", format="%d%B%Y")

wto1[which(wto1$docid=="WTCOMTDM34"),
     "date"] <- as.Date("22May2001", format="%d%B%Y")

sapply(wto1, function(x) sum(is.na(x)))

######

save(wto1, file="wtoTDMinutes.Rdata")
