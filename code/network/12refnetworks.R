## script to generate a co-occurence network
## of speaker-> referenced countries in paragraphs

## Update 11/19: need to remove loops
## those are paragraphs where the "reference"
## to the country is that country's rep speaking

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
        { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

packs=c('dplyr', 'ggnetwork', 'ggplot2',
    'readr', 'stringr', 'tnet',
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

## laptop
dataPath <- "~/Dropbox/WTO/data/ner/"


data <- read.csv(paste0(dataPath,"WTODataNew.csv"),
                 header=FALSE, stringsAsFactors=FALSE)

data$V6 <- as.Date(data$V6, format="%m/%d/%y")

length(which(data$V5==data$V8)) ## 2979

loops <- which(data$V5==data$V8)

data <- subset(data, !(data$V5==data$V8))

dim(data) ## 10832

## the other entry point of noise is where
## a country "references itself"
## which is really when "the speaker xx." also references
## other countries. Pref to take that out via the graph simplify call
## because is useful info

refs <- which(data$V5 %in% data$V8) 

## Clean up some variables:

##Create a column for year:

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

dim(refTotal) #290x3

head(refTotal)
refTotal$. <- as.character(refTotal$.)

## Changes in frequency of references to the US:

USRefs <- arrange(refTotal[which(refTotal$.=="United States"),], year)

USRefs

plot(USRefs$Freq~USRefs$year)

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

write.csv(roster,
          file=paste0(dataPath, "WTOrefDyads.csv"))

## just connections:

list <- c("V5", "year", "refs")

edges <- roster[,list]

dim(edges)  ##8016

## Create yearly graphs
## with edges
## and some edge metadata
## edgelist: V5 & refs

## also a meta dataframe of the structural variables
allnodes <- data.frame()

for(y in unique(roster$year)){
    
    edges <- as.matrix(roster[which(roster$year==y),
                          c("V5", "refs")])
    
    ## ugly character clean:
    edges <- gsub('[^a-zA-Z0-9.]', '', edges)

    ## Make graph:
    
    edges <-  graph_from_edgelist(edges,
                                  directed=TRUE)
  
    ## keep only connected component:
    edges<- delete_vertices(edges, which(degree(edges) < 1))

    ## remove loops:
    edges <- simplify(edges)
    ## Write in some metadata:
    V(edges)$outdeg <- degree(edges, mode="out")
    V(edges)$indeg <- degree(edges, mode="in")
    V(edges)$delta <- V(edges)$outdeg - V(edges)$indeg
    V(edges)$between <- betweenness(edges)
    V(edges)$comm <- cluster_walktrap(edges, steps=4)
    #V(edges)$alphacent <- alpha_centrality(edges)
    ## V(edges)$authorcent <- authority_score(edges) #just the numbers
   # V(edges)$powercent <- bonpow(edges)

    nodes <- cbind(as_data_frame(edges, "vertices"), y)
    
    ## write data
    assign(paste0("edges", y), edges)
    ## send a message out
    print(paste0("done with year ", y))

    ## update dataframe:
    allnodes <- rbind(allnodes, nodes)
}

class(allnodes)

yearlyGraphObjects <- ls(pattern="*edges")
## Save the yearly objects:

save(list=yearlyGraphObjects,
     file="yearlyWTORefGraphs.Rdata")

allnodes <- apply(allnodes,2,as.character) ## fixes an encoding error

write.csv(allnodes,
     file="nodedeltas_all.csv")
