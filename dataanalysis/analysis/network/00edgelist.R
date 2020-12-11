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

metadatainfo <- c("wbstats")

loadPkg(c(packs2, metadatainfo))

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


### Merge in IS0C3, for WBStats

metacols <- c("country", "region_iso3c",
              "iso3c","income_level_iso3c" )

meta.inc <- wb_cachelist$countries[,metacols]

head(meta.inc)

## Now want to convert edgeL into iso3c:
## Add WB metdata about:

## Sender:
tp <- merge(x=edgeL,
      y=meta.inc,
      by.x="sender",
      by.y="country")

colnames(tp)[colnames(tp)=="iso3c"] <- "sender.iso3c"
colnames(tp)[colnames(tp)=="region_iso3c"] <- "sender.region"
colnames(tp)[colnames(tp)=="income_level_iso3c"] <- "sender.il"

colnames(tp)
## Target:
edgeL2 <- merge(x=tp,
      y=meta.inc,
      by.x="target",
      by.y="country")

colnames(edgeL2)
colnames(edgeL2)[colnames(edgeL2)=="iso3c"] <- "target.iso3c"
colnames(edgeL2)[colnames(edgeL2)=="region_iso3c"] <- "target.region"
colnames(edgeL2)[colnames(edgeL2)=="income_level_iso3c"] <- "target.il"

colnames(edgeL2)

head(edgeL2)

#### Now want:

## Indicate if edges span regions
edgeL2$spanr <- NA

edgeL2[edgeL2$sender.region==
      edgeL2$target.region,]$spanr <- "no"

edgeL2[edgeL2$sender.region!=
       edgeL2$target.region,]$spanr <- "yes"

table(edgeL2$spanr) ## no 552, yes 1604 (interesting!)

##if edges span income categories
edgeL2$spaninc <- NA

edgeL2[edgeL2$sender.il==
      edgeL2$target.il,]$spaninc <- "no"

edgeL2[edgeL2$sender.il!=
       edgeL2$target.il,]$spaninc <- "yes"

table(edgeL2$spaninc) ## no 691; yes 1465

## What kindof income-cross references do we have?
## What does this tell me?
## HIC->HIC is the most common
##Then HIC->LMC
## Then HIC -> UMC; LMC-> LMC; and LMC0> HIC
## And references to and from LICs are relatively minor
table(edgeL2$sender.il,
      edgeL2$target.il)

### heatmap by year:

incRefs<- data.frame()

for(y in unique(edgeL2$years)){
    print(y)
    
    tmp <- as.data.frame(table(edgeL2[edgeL2$year==y,]$sender.il,
                             edgeL2[edgeL2$year==y,]$target.il))
    tmp <- cbind(tmp, y)
    head(tmp)
}

head(regionRefs)
    
###########
## Region cross-refernces?

as.data.frame(table(edgeL2$sender.region,
      edgeL2$target.region))


### Confirm no missing codes:
sum(is.na(edgeL2$iso3cSender)) ## no NA
sum(is.na(edgeL2$iso3cTarget)) ## no NA

#####

colnames(edgeL2)





#############################

## Put edgelist in correct order:


col.order <- c("iso3cSender", "iso3cTarget",
               "doc_id", "year")

edgeL2 <- edgeL2[,col.order]

head(edgeL2)


##########################
#### Convert to graph objects:
#########################

## Dataframe of node-level metadata:
statesNeeded <- unique(c(edgeL2$iso3cSender, edgeL2$iso3cTarget))

length(statesNeeded) ## 128


metadf <- meta.inc[meta.inc$iso3c %in% statesNeeded,]

dim(metadf) ## 128x 4

metadf <- metadf[,c("iso3c", "region_iso3c",
                    "income_level_iso3c","country")]

head(metadf)
dim(metadf)


### Tester area:

m <- "WTCOMTDM103"
tmpE <- edgeL2[edgeL2$doc_id==m,]

colnames(tmpE)

tmpE2 <- merge(tmpE,
               metadf,
               by.x="iso3cSender",
               by.y="iso3c")

### TODO: proportion of out-region, out-income group
## etc from the non-igraph data.
## then can use graph-level assortivity

tmpV <- metadf[metadf$iso3c %in% unique(c(tmpE$iso3cSender,
                                          tmpE$iso3cTarget)),]


tmpE[tmpE$iso3cSender=="USA",]

tmpV <- cbind(tmpV, m)

tmpgr <- igraph::graph_from_data_frame(tmpE,
                                       directed=TRUE,
                                       vertices=tmpV)

V(tmpgr)

vertex_attr(tmpgr) ## good

V(tmpgr)$year
as.factor(V(tmpgr)$region_iso3c)
V(tmpgr)$country

V(tmpgr)$region_iso3c[58]

## Assortivity:

unique(V(tmpgr)$name)

tst <- assortativity_nominal(tmpgt,
                      as.factor(V(tmpgr)$region_iso3c),
                      directed = TRUE)

V(tmpgr)$assortRegion

## Extract node-level attributes from the co-reference network
## worthwhile probably to also read-in:
## Region, income, level, etc into the
## graph, so that we can look for
## patterns inassortivitiy
###subset the data, generate the graph,
## extract the data, and merge back in
## then wrap that into a for-loop


nodeAttributes <- data.frame()


for(m in unique(edgeL2$doc_id)){

    tst <- edgeL2[edgeL2$doc_id==m,]
    
    gtest <- graph_from_data_frame(tst,
                                   directed=TRUE,
                                   vertices= )
    
    V(gtest)$betcent <- betweenness(gtest,
                                    directed=TRUE)
    
### ID metric quantiles:
    V(gtest)$betclass <-"bottom50"
    V(gtest)$betclass[V(gtest)$betcent>=
                          quantile(V(gtest)$betcent, c(.5))] <- "top50"
    V(gtest)$betclass[V(gtest)$betcent>=
                              quantile(V(gtest)$betcent, c(.8))] <- "top20"
    V(gtest)$betclass[V(gtest)$betcent>=
                              quantile(V(gtest)$betcent, c(.9))] <- "top10"
    
### Send out
    out <- cbind(igraph::as_data_frame(gtest, what="vertices"), m)
    edgeandats <- rbind(edgeandats, out)
}

dim(edgeL2) ## 3478 x4
dim(edgeandats) ## 2045x4

colnames(edgeL2)

dim(out)
out

### todo:
## print out quantiles;
## make a table of members in each quantile
## scale up to the meetings
## write metadata out


