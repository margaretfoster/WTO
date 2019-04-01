## script to generate a co-occurence network
## of speaker-> referenced countries in paragraphs


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
        { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

packs=c('dplyr', 'ggnetwork', 'ggplot2', 'readr', 'stringr', 'tnet',
        'network')

        
loadPkg(packs)

## co-reference network data:
## no named columns
##V2= comm
## v3= paragraph
## v4= text
## V5= speaker
##v6 = date
##V8 = referenced in the text

data <- read.csv("../../data/WTODataNew.csv",
                 header=FALSE, stringsAsFactors=FALSE)

data$V6 <- as.Date(data$V6, format="%m/%d/%y")


## Clean up some variables:

##create a year
summary(data$V6)

library(lubridate)
data$year <- year(data$V6)
summary(data$year)


## Make the empty cells a "none" rather than a space

data$V8[which(data$V8=="")] <- "None"

## Drop the data with "None"

data <- subset(data, V8!="None") ## result= 6599x9

## subset to work with:
## 2005
subset <- data[which(data$year==2005),]
dim(subset)

summary(subset$V5)
## which countries are the most common:
## produces an 82x 2 dataframe
## with entity and number of references.

sums <- data.frame()
for(i in unique(data$year)){
    x <-  table(summary(data[which(data$year==i),"V5"]))
    sums <- rbind(sums, x)
}

dim(sums)
sums


as.data.frame(table(summary(data[which(data$year==2005),"V5"])))


commonReferences <-  str_split(subset$V8,",") %>%
    trimws() %>%
    unlist %>%
        table %>%
        data.frame(stringsAsFactors=FALSE) %>%
        arrange(-Freq) %>%
        filter(Freq > 2) ## ugly hack to get rid of the
## few entries that didn't stringsplit well. COME BACK AND FIX

##Common References by year:

refTotal <- data.frame()
for(y in unique(data$year)){
    subset <- data[which(data$year==y),]
    commonReferences <-  str_split(subset$V8,",") %>%
        trimws() %>%
        unlist %>%
        table %>%
        data.frame(stringsAsFactors=FALSE) %>%
        arrange(-Freq) %>%
        filter(Freq > 2) ## ugly hack to get rid of the
    ## few entries that didn't stringsplit well. COME BACK AND FIX
    commonReferences$year <- y
    refTotal <- rbind(refTotal, commonReferences)
}

dim(refTotal) #571x3

head(refTotal)
refTotal$. <- as.character(refTotal$.)

## Changes in frequency of references to the US:

USRefs <- arrange(refTotal[which(refTotal$.=="United States"),], year)

plot(USRefs)

## Singapore is very frquently  referenced in 1996

arrange(refTotal[which(refTotal$.=="Singapore"),], year)
## also Switzerland
arrange(refTotal[which(refTotal$.=="Switzerland"),], year)


## export data to graph on laptop
## because ggplot works
write.csv(refTotal, file="refTotal.csv")


## Graphs:

## this takes the list of countries
## makes a single row per country that referenes
## another country
##
library(tidyr)

roster <- data %>%
    unnest(refs = str_split(V8, ","))

## just connections:

list <- c("V5", "year", "refs")

edges <- roster[,list]

dim(edges)  ##10995x3


## edgelist: V5 & refs
##
  
for(y in unique(roster$year)){
    
    edges <- as.matrix(roster[which(roster$year==y),
                          c("V5", "refs")])
    
    ## ugly character clean:
    edges <- gsub('[^a-zA-Z0-9.]', '', edges)

    edges <-  graph_from_edgelist(edges,
                                  directed=TRUE)
    ## make graphs:
    ## write data
    assign(paste0("edges", y), edges)
    ## send a message out
    print(paste0("done with year ", y))
}

yearlyGraphObjects <- ls(pattern="*edges")
## Save the yearly objects:


save(list=yearlyGraphObjects, file="yearlyWTORefGraphs.Rdata")
