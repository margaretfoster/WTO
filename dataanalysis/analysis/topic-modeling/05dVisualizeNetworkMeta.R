
### This script to generate summary
## visualizations of the WTO TDM Metadata

 ######################
## Declare Data Paths
######################

if(Sys.info()['user']=="Ergane"){## desktop                                                                                                                          
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine                                                                                                                                          
    dataPathDesktop <- "../../"
    print(paste0("On remote, data path is ", dataPathDesktop))
}

library(stm)
library(stringr)
library(lubridate)
library(ggplot2)
library(devtools)
library(tidystm)
###################################
## Load model
###################################
                
####################################
#### K20
####################################

load(paste0(dataPathDesktop, "tradDevPara_20InteractNetActSend.RData"))

ls()

## Here want to work with "meta"
rm(mod.out.20.rec, mod.out.20.send, prep.20.rec, prep.20.send)

ls()


colnames(meta)

colsOut <- c("date", "docid", "numsends",
             "numrefs", "firstent", "region",
             "income_level_iso3c")
sub <- meta[, colsOut]

dim(sub) ##8659

sub2 <- unique(sub)

dim(sub2) ## 2843

head(sub2)

### 7 gets into the top 20% sends
### 
########### Remember quantiles

## subset 3: with no date info:

slice3 <- c("numsends", "numrefs",
            "firstent", "region",
            "income_level_iso3c",
            "iso3c", "year", "date",
            "docid")

sub3 <- unique(meta[,slice3])

dim(sub3) ## 991x6

head(sub3)

length(unique(sub3$firstent)) ## 197

head(meta[meta$"top20prec"==TRUE,])

#########################
## Plot number of references:
#########################
sub3 <- sub3[order(sub3$numrefs),]

head(sub3)

sub3$namelevs <- paste0(sub3$iso3c, 1:nrow(sub3))

head(sub3)
tail(sub3)

gg1 <- ggplot(sub3, aes(y=numrefs,
                        x=factor(namelevs,
                                 namelevs),
                        color=region))+
    geom_point()+
    geom_text(data=sub3[sub3$numrefs>10,],
              aes(label=iso3c),
              check_overlap=TRUE)+
    theme_bw()+
    scale_x_discrete(NULL)

gg1

#########################
## Graph out references
########################

### First all at one:

library(ggplot2)

outliersIn <- sub2[sub2$numrefs > 10,] ## 25x7

gg <- ggplot(sub2, aes(x=date, y=numrefs,
                       label=firstent)) +
    geom_point(aes(colpr=region))+
    theme_bw()+
    geom_text(data=outliersIn,
              check_overlap=TRUE)
  
gg
