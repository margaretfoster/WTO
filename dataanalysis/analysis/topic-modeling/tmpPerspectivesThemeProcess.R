### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda',
           "plm", 'sandwich',
           "stringi")

loadPkg(packs)

ls()
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
load("twoTopicsAndSubSets-NoAdminSubset_Alt.Rdata")

## For summary statistics;
## see the themSubsetTopic2_Alt.R script

#####################################
##### Subset: Topic One-- "Process" Frame
######################################
summary(mod.out.2) ## 
mod.out.2.meta <- out$meta

## Extract paragraph-level topic assignments

theta2.out <- as.data.frame(round(mod.out.2$theta, 2))  ## round to two
colnames(theta2.out) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(
                                 theta2.out)))

theta2.out <- cbind(out$meta$pid, theta2.out)

#### Assign dominant topic:
theta2.out$assignedtopic <- colnames(theta2.out[,2:3])[apply(
                                theta2.out[,2:3],1,which.max)] 

table(theta2.out$assignedtopic)

round(prop.table(table(theta2.out$assignedtopic)),2)

head(theta2.out)

paraTopicsK2 <- merge(x =mod.out.2.meta,
                    y=theta2.out,
                    by.x="pid",
                    by.y="out$meta$pid")

dim(paraTopicsK2) ## 5228 x 19

colnames(paraTopicsK2)

head(paraTopicsK2)

############################
## Identify the theorized shocks
## In the second theme (Programming)
############################

## Subset to Topic 1:

## Process-dominant paragraphs:
processParas <- paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="M2.Topic1"),]

dim(processParas) ##1715 x 29

## Subset One: Clean the Process paragraphs

processParas$cleanedtext <- gsub("[[:digit:]]", "",
                                  processParas$paratext)

processParas$cleanedtext <- gsub("[[:punct:]]", "",
                                  processParas$paratext)

data2 <- corpus(processParas, text_field = 'paratext')
docvars(data2)$text <- as.character(data2)

length(data2)

dat2 <- dfm(data2, stem = TRUE,
            remove = stopwords('english'),
            remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 2)

out <- convert(dat2, to = 'stm')

round((table(out$meta$"income_level_iso3c")), 2)
round(prop.table(table(out$meta$"income_level_iso3c")), 2)

presenters <- as.data.frame(table(out$meta$firstent))
presenters <- presenters[order(presenters$Freq),]

dim(presenters) ##77
                            
#### Model sub-themes

## For identification of redistributors and
## reciprocators, see draft appendix

recip <- c("European Union", "Canada",
           "Japan", "Switzerland",
           "Trinidad and Tobago",
           "United States")

redist <- c("Argentina","Bangladesh", "Bolivia",
            "Brazil", "Cambodia", 
            "China", "Cote d'Ivoire",
            "Cuba", "Dominca", 
            "Egypt", "El Salvador",
            "Fiji", "Guatemala",
            "India", "Jamaica",
            "Kenya", "Lesotho", "Malaysia",
            "Mexico", "Morocco",
            "Poland", "Sri Lanka",
            "Uganda", "Uruguay",
            "Zambia")


## Also more stripped-down factions:

faction1 <- c("European Union", "Canada",
              "United States")

faction2 <- c("China", "Egypt", "India")


##Faction
out$meta$faction <- "Other"
out$meta[which(
    out$meta$firstent %in% faction1),
         "faction"] <- "US-EU-Can"

out$meta[which(
    out$meta$firstent %in% faction2),
         "faction"] <- "China-Egypt-India"

## redist-recipro
out$meta$cat <- "Other"
out$meta[which(
    out$meta$firstent %in% recip),
         "cat"] <- "Reciprocators"

out$meta[which(
    out$meta$firstent %in% redist),
         "cat"] <- "Redistributors"


out$meta$faction <- as.factor(out$meta$faction)
out$meta$cat <- as.factor(out$meta$cat)


###
faction.model= ~s(meetingno)+ faction
category.model= ~s(meetingno)+ cat

#### Theme Cluster Models, Topic 1:

set.seed(6889)

## Faction breakout:

mod.out.themefaction <- stm(documents=out$documents,
                            vocab=out$vocab,
                            data=out$meta,
                            K=5, ## 
                            prevalence=faction.model,
                            content= ~faction,
                            seed=61921)

prep.out.themefaction <- estimateEffect(c(1:5) ~s(meetingno)+ faction,
                                        mod.out.themefaction,
                                        metadata=out$meta,
                                        documents=out$documents,
                                        uncertainty=c("Global"))

summary(mod.out.themefaction)

## Redistributor/Reciprocator

mod.out.themecat <- stm(documents=out$documents,
                        vocab=out$vocab,
                        data=out$meta,
                        K=5, ## 
                        prevalence=category.model,
                        content= ~cat,
                        seed=61921)

prep.out.themecat <- estimateEffect(c(1:5) ~s(meetingno)+ cat,
                                    mod.out.themecat,
                                    metadata=out$meta,
                                    documents=out$documents,
                                    uncertainty=c("Global"))

summary(mod.out.themecat)


## make.dt() combines doc-topic loadings (theta) +  metadata 

save.image(file="ProcessSubSetstm.Rdata")
