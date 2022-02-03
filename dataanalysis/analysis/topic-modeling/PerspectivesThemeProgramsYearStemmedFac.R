### Preliminary analysAis of trade and development
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

ls()
colnames(out$meta)
## For summary statistics
## see K2ClassificationModel.R

#####################################
##### Subset: Topic One-- "Process" Frame
######################################

ls()

mod.out.2.meta <- out$meta

## Extract paragraph-level topic assignments

## First Category, then Faction

theta2.out.fac <- as.data.frame(round(mod.out.2$theta, 2))  ## round to two

colnames(theta2.out.fac) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(
                                 theta2.out.fac)))

theta2.out <- cbind(out$meta$pid, theta2.out.fac)

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

############################
## Identify the theorized shocks
## In the second theme (Programming)
############################

## Subset to Topic 2:
## Programs-dominant paragraphs:
programsParas <- paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="M2.Topic2"),]

dim(programsParas) ##2630 x 31

colnames(programsParas)

 
## Subset One: Clean the Programs paragraphs

programsParas$cleanedtext <- gsub("[[:digit:]]", "",
                                  programsParas$paratext)

programsParas$cleanedtext <- gsub("[[:punct:]]", "",
                                  programsParas$paratext)

data2 <- corpus(programsParas, text_field = 'paratext')
docvars(data2)$text <- as.character(data2)

length(data2)

dat2 <- dfm(data2,
            stem = TRUE,
            remove = stopwords('english'),
            remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 2)

out <- convert(dat2, to = 'stm')

colnames(out$meta)

dim(out$meta) ## 2598

###
## Model on specific subset:

faction.model= ~s(year)*faction

set.seed(6889)

## Faction breakout:

mod.programs.themefaction <- stm(documents=out$documents,
                            vocab=out$vocab,
                            data=out$meta,
                            K=10, ## 
                            prevalence=faction.model,
                            content= ~faction,
                            seed=61921)

prep.programs.themefaction <- estimateEffect(c(1:10) ~s(year)* faction,
                                        mod.programs.themefaction,
                                        metadata=out$meta,
                                        documents=out$documents,
                                        uncertainty=c("Global"))

summary(mod.programs.themefaction)

save.image(file="programsSubSetstmYearStemmedFac.Rdata")
