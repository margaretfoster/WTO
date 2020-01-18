## Script to make a network out of the WTO 1 data

rm(list=ls())

library(reshape2)
library(xtable)

dataPath<- "./"

## wto1 is the dataframe, it has speakers extracted for each
## paragraph, dates extracted, full text for the paragraph 
##  meeting ID and paragraph #

load("wtoTDMinutes.Rdata")

ls()

class(wto1)
dim(wto1)

colnames(wto1)

## remove unused column:o
wto1$extradates <- NULL

## ensure date a real date:
wto1$date <- as.Date(wto1$date, format="%d%B%Y")
class(wto1$date)

wto1$numdate <- as.numeric(wto1$date)

#################################
## Adjacency matrix
################################

### Matrix 1: Which countries speak in the same meetings?
### Dcast docid ~ country.speaker

wto2 <- dcast(wto1, docid~country.speaker)

## move docid to rownames:
## and drop docid column
rownames(wto2) <- wto2$docid
wto2$docid <- NULL
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

write.csv(wto1,
          file="WTOData.csv")

write.csv(forExport,
          file="simple-shared-meeting-adj.csv")
