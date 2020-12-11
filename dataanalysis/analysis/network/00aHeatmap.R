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


colnames(wb_cachelist$countries)
### Merge in IS0C3, for WBStats

metacols <- c("country", "region_iso3c", "region",
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
colnames(tp)[colnames(tp)=="region"] <- "sender.region"
colnames(tp)[colnames(tp)=="income_level_iso3c"] <- "sender.il"

colnames(tp)
## Target:
edgeL2 <- merge(x=tp,
      y=meta.inc,
      by.x="target",
      by.y="country")

colnames(edgeL2)
colnames(edgeL2)[colnames(edgeL2)=="iso3c"] <- "target.iso3c"
colnames(edgeL2)[colnames(edgeL2)=="region"] <- "target.region"
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

    
###########
## Region references:
colsToPivot <- c("sender.region", "target.region", "year")

edgeL3int <- edgeL2[,colsToPivot]

dim(edgeL3int) ## 2156 ties


### counts region-region references by year:

edgeL3 <- edgeL3int%>%
    group_by(sender.region, target.region, year)%>%
    summarize(n=n())

head(edgeL3)
dim(edgeL3) ##636x 4

edgeL3$sender.region <- as.factor(edgeL3$sender.region)
levels(edgeL3$sender.region)


#### Plot:

ggdat1 <- edgeL3[edgeL3$year %in% c(1995:2003),]
ggdat2 <- edgeL3[edgeL3$year %in% c(2004:2013),]
ggdat3 <- edgeL3[edgeL3$year %in% c(2014:2019),]


##Todo: make 0=gray; invert colors so darker /more red = more referenes
## make plots for more years (code below is 1995-2003)
### and scale plot for income-income references
ptst1 <- ggplot(edgeL3, aes(y=edgeL3$sender.region,
                            x=edgeL3$target.region,
                            fill=edgeL3$n))+
    geom_tile()+
    facet_wrap(edgeL3$year)+
 ##   theme_bw() + ## actually want gray for the holes in the graph
    labs(fill = "Refs")+
    scale_fill_gradient(low="white", high="red",na.value = "grey") +
    theme(axis.text.x = element_text(angle = 45))+
    scale_y_discrete(limits = rev(levels(edgeL3$sender.region)))+    
     labs(x="Target Region",
         y="Sender Region",
         title="Region-Region References on WTO TD Committee")

ptst1
ggsave(file="regionRefs.pdf", device='pdf')

########################
### Cross income references
## Region references:

colnames(edgeL2)

colsToPivot <- c("sender.il", "target.il", "year")

edgeL4nt <- edgeL2[,colsToPivot]

dim(edgeL4nt) ## 2156 ties

head(edgeL4nt)


### Category References by year:

edgeL4 <- edgeL4nt%>%
    group_by(sender.il, target.il, year)%>%
    summarize(n=n())

head(edgeL4)
dim(edgeL4) ##287

## Reorder levels:
edgeL4$sender.il <- as.factor(edgeL4$sender.il)
edgeL4$target.il <- as.factor(edgeL4$target.il)

incLevels <- c("LIC", "LMC", "UMC", "HIC")

levels(edgeL4$sender.il) <- rev(incLevels)
levels(edgeL4$target.il) <- rev(incLevels)

levels(edgeL4$target.il)

#### Plot:

ggdat1 <- edgeL4[edgeL4$year %in% c(1995:2003),]
ggdat2 <- edgeL4[edgeL4$year %in% c(2004:2013),]
ggdat3 <- edgeL4[edgeL4$year %in% c(2014:2019),]


##Todo: make 0=gray; invert colors so darker /more red = more referenes
## make plots for more years (code below is 1995-2003)
### and scale plot for income-income references
ptst1 <- ggplot(edgeL4, aes(y=edgeL4$sender.il,
                            x=edgeL4$target.il,
                            fill=edgeL4$n))+
    geom_tile()+
    facet_wrap(edgeL4$year)+
##    theme_bw() +
    scale_fill_gradient(low="white", high="red",
                        na.value = "grey") +
    theme(axis.text.x = element_text(angle = 45))+
    labs(fill = "Refs.")+
    scale_y_discrete(limits = rev(levels(edgeL4$sender.il)))+    
     labs(x="Target",
         y="Sender",
         title="References on WTO TD Committee By Country Income")

ptst1
ggsave(file="incomeRefs.pdf", device='pdf')

