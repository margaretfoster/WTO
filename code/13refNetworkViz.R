## script to generate a co-occurence network
## of speaker-> referenced countries in paragraphs
 rm(list=ls())

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

## Processing data:
## frequent senders:
freqsends <- data.frame()
for(y in unique(data$year)){
    x <- as.data.frame(cbind(table(
        summary(data[which(data$year==y),"V5"])),y))
    x$country <- rownames(x)
    rownames(x) <- 1:dim(x)[1]
    freqsends <- rbind(freqsends, x)
}

colnames(freqsends) <- c("paracount", "year", "country")

dim(freqsends) ##1019 x3

head(freqsends)


## load the yearly objects:

load("yearlyWTORefGraphs.Rdata")

ls()

edges2005

df2005e <- as_data_frame(edges2005, what="edges")
df2005n <- as_data_frame(edges2005, what="vertices")

class(df2005)

head(df2005n)
head(df2005e)

table(df2005e[which[],])

c2009 <- cluster_walktrap(edges2009,
                          steps = 2,
                          merges = TRUE,
                          modularity = TRUE,
                          membership = TRUE)

c2009 ## generates 4 groups


### Visualizations


##1995
pdf(file="1995Edges.pdf")
plot(edges1995,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")

dev.off()
##1996

pdf(file="1996Edges.pdf")
plot(edges1996,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
 
##1997
pdf(file="1997Edges.pdf")
plot(edges1997,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
## 1998
pdf(file="1998Edges.pdf")
plot(edges1998,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

## 1999

pdf(file="1999Edges.pdf")
plot(edges1999,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
##2000

pdf(file="2000Edges.pdf")
plot(edges2000,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

## 2001

pdf(file="2001dges.pdf")
plot(edges2001,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2002

pdf(file="2002Edges.pdf")
plot(edges2002,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
##2003
pdf(file="2003Edges.pdf")
plot(edges2003,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

## 2004
pdf(file="2004Edges.pdf")
plot(edges2004,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2005

pdf(file="2005Edges.pdf")
plot(edges2005,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2006


vizfunction <- function(graphobj, filename){
    
    colvec <- ifelse(V(graphobj)$delta >= 0,
                     "deeppink", "lightblue")
    ## size of country names is .5 if in top
    ## 75% of in-degree (ie: referred to)
    ## for that year, else .25
    labsiz <- ifelse(quantile(V(graphobj)$indeg)[4],
                     0.5 , 0.25)

    set.seed(6889)
    pdf(file=filename)
    plot(simplify(graphobj),
         vertex.size=log(abs(V(graphobj)$delta))+.5,
         edge.arrow.size=0.25,
         vertex.label.dist=.1,
         vertex.label.cex=labsiz,
         vertex.color= colvec)
    dev.off()
}

## entry of china (2000, 2001, 2002, 2003)
vizfunction(edges2000,"2000HeirarchyGraph.pdf" )
vizfunction(edges2001,"2001HeirarchyGraph.pdf" )
vizfunction(edges2002,"2002HeirarchyGraph.pdf" )

## after the financial crisis (2008, 2009)
vizfunction(edges2008,"2008HeirarchyGraph.pdf" )
vizfunction(edges2008,"2009HeirarchyGraph.pdf" )

##2007

pdf(file="2007Edges.pdf")
plot(edges2007,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
##2008

pdf(file="2008Edges.pdf")
plot(edges2008,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2009

pdf(file="2009Edges.pdf")
plot(edges2009,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
##2010

pdf(file="2010Edges.pdf")
plot(edges2010,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2011
pdf(file="2011Edges.pdf")
plot(edges2011,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()



##2012

pdf(file="2012Edges.pdf")
plot(edges2012,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2013
pdf(file="2013Edges.pdf")
plot(edges2013,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
##2014

pdf(file="2014Edges.pdf")
plot(edges2014,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()

##2015
pdf(file="2015Edges.pdf")
plot(edges2015,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="lightgreen")
dev.off()
## 2016
pdf(file="2016Edges.pdf")
plot(edges2016,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="green")
dev.off()


pdf(file="2018Edges.pdf")
plot(edges2018,
     vertex.size=7,
     edge.arrow.size=0.25,
     vertex.color="pink")
dev.off()


## ## batch the graph

## pdf(file=paste0(y,"Edges.pdf"))
## plot(edges,
##      vertex.size=7,
##      edge.arrow.size=0.25,
##      vertex.color="lightgreen")
## dev.off()

