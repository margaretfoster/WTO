### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## This specification: Model covariates by year with faction
## content covariates

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

if(Sys.info()['user']=="Ergane" |
   Sys.info()['user']=="Promachos"){ ## if on my own machines look in Dropbox
    print(Sys.info()['user'])
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
#############################

load("twoTopicsAndSubSets-NoAdminSubset_CatFac.Rdata")

attributes(mod.out.2) ## K=2, model= ~(year)

attributes(mod.out.2$settings$call)

## Summary of number of paragraphs in each topic:

theta.summary<- as.data.frame(round(mod.out.2$theta, 2))  ## round to two

colnames(theta.summary) <- gsub(pattern="V",
                                replace="M2.Topic",
                                x=as.character(colnames(
                                    theta.summary)))

theta.summary <- cbind(out$meta$pid, theta.summary)

head(theta.summary)

## get top topic
theta.summary$assignedtopic <- colnames(
    theta.summary[,2:3])[apply(
        theta.summary[,2:3],1,which.max)] 

theta.summary[which(theta.summary$assignedtopic=="M2.Topic1"),
              "assignedtopic"] <- "Process"

theta.summary[which(theta.summary$assignedtopic=="M2.Topic2"),
              "assignedtopic"] <- "Programs"

## Report: Descrptives: Proportion of topics 
table(theta.summary$assignedtopic)
round(prop.table(table(theta.summary$assignedtopic)),2)

head(theta.summary)

## Bring in metadata
paraTopicsK2 <- merge(x=out$meta,
                    y=theta.summary,
                    by.x="pid",
                    by.y="out$meta$pid")

dim(paraTopicsK2) ## 5228 x 19

## Report: Tech Appendix Figure Overview of
## Dominant Themes

# Plot the model:
twoTopics <- c("Process", "Programs")

## Custom plot legend:
commas <- function(text){  
  paste(text[nchar(text)>0], collapse=", ")
}

label.t1 <- paste0("Process: ",
                   commas( ## paste frex
                       labelTopics(mod.out.2, n=5)$frex[1,]))

label.t2 <- paste0("Programs: ",
                   commas(
                       labelTopics(mod.out.2, n=5)$frex[2,]))



png(file="overallThemesK2byyear.png")
plot.estimateEffect(prep.2,
                    model=mod.out.2,
                    covariate="year",
                    main="CTD Corpus Dominant Themes",
                    topics=c(1:2), 
                    method="continuous",
                    xlab="Year",
                    labeltype="custom",
                    printlegend=FALSE,
                    n=5,
                    linecol=c("red", "darkblue")
                    )
legend("topright", legend=c(label.t1, label.t2),
       col=c("red", "darkblue"), lty=1)
dev.off()



############################
##### Analysis
############################

