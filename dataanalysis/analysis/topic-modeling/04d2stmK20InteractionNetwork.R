### Preliminary analysis of trade and development
## Using speaker-level metadata about network activity


rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

############################
##### Analysis
############################

colnames(out$meta)

table(out$meta$NetActInRef)

## how many referencs are we talking?
## top 10% per meeting:

top10 <- out$meta[out$meta$NetActInRef=="Top10P", ]


## who:
sink(file="~/Dropbox/WTO/top10p.txt")
summary(top10$numrefs) ##7-10
unique(top10$firstent)
sink()
##



#### top 5%
top05 <- out$meta[out$meta$NetActInRef=="Top05P", ]

## who:
sink(file="~/Dropbox/WTO/top05p.txt")
summary(top05$numrefs) ##11-26
unique(top05$firstent)
sink()

## top 20:

top20 <- out$meta[out$meta$NetActInRef=="Top20P", ]
summary(top20$numrefs) ##5-6

## who:
sink(file="~/Dropbox/WTO/top20p.txt")
summary(top20$numrefs) ##11-26
unique(top20$firstent)
sink()

#####################
## Who are in the out-references list?


top10out <- out$meta[out$meta$NetActSend=="Top10P", ]


## who:
sink(file="~/Dropbox/WTO/top10pout.txt")
summary(top10out$numrefs) ##7-10
unique(top10out$firstent)
sink()
##


#### top 5%
colnames(out$meta)

top05out <- out$meta[out$meta$NetActSend=="Top05P", ]

dim(top05out)

top05out[which(top05out$firstent=="Zambia"), "docid"]## M4
         
## who:
sink(file="~/Dropbox/WTO/top05pout.txt")
summary(top05out$numrefs) ##11-26
unique(top05out$firstent)
sink()

## top 20:

top20out <- out$meta[out$meta$NetActSend=="Top20P", ]
summary(top20out$numrefs) ##5-6

## who:
sink(file="~/Dropbox/WTO/top20pout.txt")
summary(top20out$numrefs) ##11-26
unique(top20out$firstent)
sink()

############################
#### K=20 Network Senders
############################

set.seed(61920)

mod.out.20.send <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=20, ## 
                  prevalence= ~ s(numdate) *
                  as.factor(NetActSend),
               seed=61920)
 

prep.20.send <- estimateEffect(c(1:20)~ s(numdate) *
                          as.factor(NetActSend),
                       mod.out.20.send,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))


mod.out.20.rec <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=20, ## 
                  prevalence= ~ s(numdate) *
                  as.factor(NetActInRef),
               seed=61920)
 

prep.20.rec <- estimateEffect(c(1:20)~ s(numdate) *
                          as.factor(NetActInRef),
                       mod.out.20.rec,
                       metadata=meta,
                       documents=docs,
                          uncertainty=c("Global"))


save.image(file=paste0(dataPathDesktop,
               "tradDevPara_20InteractNetActSend.RData"))
