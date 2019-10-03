loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
        { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

packs=c('dplyr', 'igraph', 'ggnetwork',
    'ggplot2','readr', 'stringr',
    'tnet','network')


loadPkg(packs)


## load referrer graphs for WTO
## committee, one for each year
## loads igraph objects
## sender is the representative speaking
## reciever is who was mentioned

load(file="yearlyWTORefGraphs.Rdata")

ls()


plot(edges2007)

## Data that I want:
## degree, without self-loops.

test <- simplify(edges2007,
                 remove.loops = TRUE,
                 remove.multiple=FALSE)

print(edges2007)

print(edges2007, full=TRUE)

vertex_attr(edges2007)


node.info <- read.csv("~/Dropbox/WTO/data/nodedeltas_all.csv",
                      stringsAsFactors=FALSE)

class(node.info)

head(node.info)

##changes in reference patterns to Egypt?


egypt <- node.info[which(node.info$name=="Egypt"),]

egypt <- egypt[order(egypt$y),]

us <- node.info[which(node.info$name=="UnitedStates"),]
us <- us[order(us$y),]


cn <- node.info[which(node.info$name=="China"),]
cn <- cn[order(cn$y),]



ind <- node.info[which(node.info$name=="India"),]
ind <- ind[order(ind$y),]


ru <- node.info[which(node.info$name=="RussianFederation"|
                      node.info$name=="Russia"),]
ru <- ru[order(ru$y),]
ru$name <- "Russia"


### combine:


group <- rbind(us, cn, ru, ind)


p1 <- ggplot() + geom_line(aes(y = delta,
                               x = y, colour = name),
                           data = group,
                           stat="identity")
p1 <- p1 + theme_bw()

p1


p2 <- ggplot() + geom_line(aes(y = indeg,
                               x = y, colour = name),
                           data = group,
                           stat="identity")
p2 <- p2 + theme_bw()

p2

## delta references


####

##changes in centrality over time

centscores <- node.info[order(node.info$between),]

dim(centscores) ## 1653x7

centscores[1600:1653,]



p2 <- ggplot() + geom_point(aes(y=between,
                                x=y,
                                colour=name),
                            data=centscores[1600:1653,],
                            stat="identity")
p2 <- p2+theme_bw()

p2
