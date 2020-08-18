### Take the adjacency network
### Convert into a country-year or
## (ideally) country-meeting count of ingoing or outgoing ties

rm(list=ls())


loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs2 <- c("tidyr", "reshape2", "igraph",
            "dplyr", "ggplot2")

loadPkg(packs2)

###########################
#### Declare Paths
##########################

if(Sys.info()['user']=="Ergane"){## desktop
    dataPathDesktop <- "~/Dropbox/WTO/data/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine
    dataPathDesktop <- "../../"
    print(paste0("On remote, data path is ", dataPathDesktop))
}


data <- read.csv(paste0(dataPathDesktop,
                        "WTO_TDM_Reference_AjM.csv"),
                        stringsAsFactors=FALSE)

colnames(data)

## need "source" to have a different name

colnames(data)[which(colnames(data)=="source")] <- "sender"

colnames(data)


##########################
##Data overview
##########################
##################################
### remove info other than the edges

edgeL <- data[,c("sender", "target", "doc_id", "year")]

head(edgeL)

## Want to make a list of graph objects,
## by document ID
length(unique(edgeL$doc_id)) ## 105

###

graphL <- graph_from_data_frame(edgeL)

graphL ## 149x 3478 graph

coms <- cluster_walktrap(graphL)

sizes(coms) ## in the entire data, there are 84 walktrap-identiifed
## groups


membership(coms)








#### idea: generate functions that
###subset the data, generate the graph,
## extract the data, and merge back in
## then wrap that into a for-loop

## test:

tst <- edgeL[edgeL$doc_id=="WTCOMTDM83",]

dim(tst) ##58x4

gtest <- graph_from_data_frame(tst,
                               directed=TRUE)

gtest ##

