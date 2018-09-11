
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

dim(speakers) ##13812x1

speakers$V1 <- as.character(speakers$V1)

paraspath <- "../../paras//"

speakers$V1 <- gsub(paraspath, "",
                speakers$V1)

## split out the speaker country:

speakers <- separate(speakers,
                     col="V1",
                     into=c("para","speaker"),
                     sep= ".txt")

speakers$para <- as.character(speakers$para)
speakers$speaker <- as.character(speakers$speaker)

speakers[1250: 1260,]

class(speakers$para)

####################
## merge in speakers:
###################


## Merge the speaker and text:
## ensure that sets in speakers paragraphs
## and paras paragraphs are the same:

which(!(paras$V1 %in% speakers$para))
which(!(speakers$para %in% paras$V1))

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

paras2[1,]

paras2$docnum <- as.numeric(paras2$docnum)
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


dates$V1 <- gsub(filepath, " ", dates$V1)
dates$V2 <- gsub(preamble, " ", dates$V2)
dates$V1 <- gsub(".txt", "", dates$V1)

dates[1:15,]

## pull out the ones with multiple dates, by looking for commas and "AND"
## actually do these by hand. Ugly, but uglier not to:

dates$extradate <- NA

## Make sure you need these and the indexing lines up:
## dates[1, c("V2", "extradate")] <- c("24 NOVEMBER 2016", "25 NOVEMBER 2016")
## dates[2, c("V2", "extradate")] <- c("27 JANUARY 2017", "14 MARCH 2017")
## dates[9, c("V2", "extradate")] <- c("15 OCTOBER 1996", "31 OCTOBER 1996")
## dates[24, c("V2", "extradate")] <- c("7 JULY 1999", "8 JULY 1999")
## dates[28, c("V2", "extradate")] <- c("7 JUNE 2000", "10 JULY 2000")
## dates[31, c("V2", "extradate")] <- c("27 OCTOBER 2000", "8 NOVEMBER 2000")
## dates[44, c("V2", "extradate")] <- c("8 OCTOBER 2002", "9 OCTOBER 2002")
## dates[45, c("V2", "extradate")] <- c("11 NOVEMBER 2002", "18, 19, 22 NOVEMBER 2002")
## dates[47, c("V2", "extradate")] <- c("22 MAY 2003", "12 JUNE 2003")
## dates[48, c("V2", "extradate")] <- c("16 OCTOBER 2003", "23 OCTOBER 2003")
## dates[49, c("V2", "extradate")] <- c("27 NOVEMBER 2003", "28 NOVEMBER 2003")
## dates[53, c("V2", "extradate")] <- c("28 SEPTEMBER 2004", "29 SEPTEMBER 2004")
## dates[54, c("V2", "extradate")] <- c("16 NOVEMBER 2004", "26 NOVEMBER AND 10 DECEMBER 2004")
## dates[74, c("V2", "extradate")] <- c("24 MAY 1996", "7 JUNE 1996")
## dates[75, c("V2", "extradate")] <- c("24 MAY 1996", "7 JUNE 1996")
## dates[82, c("V2", "extradate")] <- c("12 OCTOBER 2009", "3 NOVEMBER 2009")
## dates[89, c("V2", "extradate")] <- c("21 JUNE 2011", "5 JULY 2011")
## dates[90, c("V2", "extradate")] <- c("14 NOVEMBER 2011", "17, 18, 25, 28 NOVEMBER 2011")
## dates[90, c("V2", "extradate")] <- c("14 NOVEMBER 2011")


## get rid of extra whitespace:

dates$V1 <- gsub(" ", "", dates$V1)

### break paras into paragraph ID, doc ID:

## first pass about whether this will throw a problem:

which(!(paras2$docid %in% dates$V1))
which(!(dates$V1 %in% paras2$docid))

df3 <- merge(paras2, dates,
             by.x="docid", by.y="V1",
             all.x=TRUE)

dim(df3)


df3[1060: 1065,]

## Drop extraneous columns:

df3$para <- NULL
df3$doc <- NULL

## Formate primary date:

df3$V2.y <- as.Date(df3$V2.y,
                    format="%d %B %Y")

colnames(df3)

colnames(df3) <- c("docid", "parnum", "paratext",
                   "country.speaker", "date", "extradates")

wtoTDMinutes <- df3

save(wtoTDMinutes, file="wtoTDMinutes.Rdata")
