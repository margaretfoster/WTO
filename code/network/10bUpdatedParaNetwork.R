## Script to make a network out of the WTO data
## Updated on 3/20 with the cleaned up NER data that Joyce
## Gave me

## Goal: monthly networks for the time that we have

rm(list=ls())

library(reshape2)
library(xtable)

dataPath<- "../../data/"

## wto1 is the dataframe, it has speakers extracted for each
## paragraph, dates extracted, full text for the paragraph 
##  meeting ID and paragraph #

nerdat <- read.csv(paste0(dataPath, "WTODataNew.csv"),
         header=FALSE)

ls()

class(nerdat)
dim(nerdat)
colnames(nerdat) <- c("comfile",
                      "comID",
                      "paranum",
                      "text",
                      "speaker",
                      "date",
                      "unknown",
                      "referenced")

## numeric fields:
numfields <- c("paranum", "unknown")

textfields <- c("comfile", "comID", "text",
                "speaker", "referenced")

## fix data class:
## Numeric columns:
for(i in numfields){
    nerdat[,i] <- as.numeric(nerdat[,i])
    }

## text columns
for(i in textfields){
    nerdat[,i] <- as.character(nerdat[,i])
    }

## date
nerdat$date2 <- as.Date(nerdat$date,
        format="%m/%d/%y")

nerdat$year <-format(as.Date(nerdat$date2,
                              format="%Y/%m/%d"),"%Y")

nerdat$month<-format(as.Date(nerdat$date2,
                             format="%Y/%m/%d"),"%m")

head(nerdat$month)
head(nerdat$year)

colnames(nerdat)

### Save data:

save(nerdat,
     file=paste0(dataPath, "NERDat.Rdata"))


#################################
## Adjacency matrix
################################

## Subset into just one meeting to work with:

## 99 x11
m72 <- nerdat[which(nerdat$comID=="WTCOMTDM72"),]

m72[,c("speaker", "referenced")]
class(m72$referenced)

m72$referenced

## 


### Matrix 1: Which countries speak in the same meetings?
### Dcast comID ~ country.speaker

##dim should be 108x 156 (meetings x speakers)

wto2 <- dcast(nerdat, comID~speaker)

colnames(wto2)
## move docid to rownames:
## and drop docid column

rownames(wto2) <- wto2$comID

wto2$comID <- NULL
## Generates data in the form of a matrix with docid and count of
## countries listed in the document

#################################
## Simple bipartite graph
################################

library(igraph)

wtobipartite <- ifelse(wto2>0, 1, 0)

rownames(wtobipartite) <- rownames(wto2)


bg=graph_from_incidence_matrix(wtobipartite)


## projection to have shared membership:

bgproj <- bipartite_projection(bg, multiplicity = TRUE)

bgproj ## 156 nodes, 5684 ties for docs that share states; bgproj$proj2 = 155 nodes 5529 


## mutual shared activity= bgproj$proj2

mutualStates <- bgproj$proj2

### Export as adjacency matrix:

forExport <- as.data.frame(as_adj(mutualStates,
                                  type="upper",
                                  sparse=FALSE))

dim(forExport)
## Save data:

write.csv(nerdat,
          file="WTOData.csv")

write.csv(forExport,
          file="simple-shared-meeting-adj.csv")

plot(bgproj$proj2)

########################
#### Yearly network

######################


colnames(nerdat)

## each year:

nerdat$year <- as.numeric(nerdat$year)
years <- unique(nerdat$year)


for(y in years){
    ## subset data into yearly dataframes:
    assign(paste0("nerdat", y),
           nerdat[which(nerdat$year==y),])
    print(paste0("done year: ", y)) 
}


ls()

### bipartite projection per year:


