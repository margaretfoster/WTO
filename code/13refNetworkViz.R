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


data <- read.csv("~/Dropbox/WTO/data/NER/WTODataNew.csv",
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
##Graph  vizualiations

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

## after trump:

vizfunction(edges2016, "2016HeirarchyGraph.pdf" )
vizfunction(edges2017, "2017HeirarchyGraph.pdf" )
vizfunction(edges2018, "2018HeirarchyGraph.pdf" )


##### communities:

wc2005 <-walktrap.community(edges2005, steps=2)
membership(wc2005)

wc2007 <-walktrap.community(edges2007, steps=2)
wc2008 <-walktrap.community(edges2008, steps=2)
wc2009 <-walktrap.community(edges2009, steps=2)
wc2010 <-walktrap.community(edges2010, steps=2)
wc2011 <-walktrap.community(edges2011, steps=2)
wc2012 <-walktrap.community(edges2012, steps=2)
wc2013 <-walktrap.community(edges2013, steps=2)
wc2014 <-walktrap.community(edges2014, steps=2)
wc2015 <-walktrap.community(edges2015, steps=2)
wc2016 <-walktrap.community(edges2016, steps=2)
wc2017 <-walktrap.community(edges2017, steps=2)
wc2018 <-walktrap.community(edges2018, steps=2)



dev.off()
set.seed(6889)

### Manually do edges:
##pdf(file="2005EdgesCommunity.pdf")



par(mfrow=c(2,2))

##Obama: 2012-2017
## Trump: 2017-


pdf(file="2015Community.pdf")
plot(simplify(edges2015),
     vertex.size=5,
     edge.arrow.size=0.15,
     vertex.label.dist=.1,
     vertex.label.cex=.65,
     main="2015 Edges",
     vertex.color= wc2015$membership)
dev.off()

pdf(file="2016Community.pdf")
plot(simplify(edges2016),
     vertex.size=5,
     edge.arrow.size=0.15,
     vertex.label.dist=.1,
     vertex.label.cex=.65,
     main="2016 Edges",
     vertex.color= wc2016$membership)
dev.off()

pdf(file="2017Community.pdf")
plot(simplify(edges2017),
     vertex.size=5,
     edge.arrow.size=0.15,
     vertex.label.dist=.1,
     vertex.label.cex=.65,
     main="2017 Edges",
     vertex.color= wc2017$membership)
dev.off()

pdf(file="2018Community.pdf")
plot(simplify(edges2018),
     vertex.size=5,
     edge.arrow.size=0.15,
     vertex.label.dist=.1,
     vertex.label.cex=ifelse(degree).65,
     main="2018 Edges",
     vertex.color= wc2018$membership)
dev.off()





vertex_attr(edges2018)
## set an attribute that is
## red if in 75% or higher of
## indegree references

set_vertex_attr(edges2018,
                name= "highIn",
                index=which(V(edges2018)$indeg >= quantile(V(edges2018)$indeg, .75)),
                value= "red")


#dev.off()

## Want visualization for 2015/2016 and then
## for 2017/2018

ls()
