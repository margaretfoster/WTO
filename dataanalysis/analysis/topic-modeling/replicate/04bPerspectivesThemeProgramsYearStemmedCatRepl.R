### Preliminary analysAis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(!lib %in% installed.packages()[,1]){
            install.packages(lib,
                             repos='http://cran.rstudio.com/')}
        suppressMessages(
            library(lib,
                    character.only=TRUE))}}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda',
           "plm", 'sandwich',
           "stringi")
loadPkg(packs)

#############################
## Load Processed Data
#############################
load("twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")

colnames(out$meta)
## For summary statistics
## see 02K2ClassificationModelRepl.R

#####################################
##### Subset: Topic One-- "Process" Frame
######################################

mod.out.2.meta <- out$meta

## Extract paragraph-level topic assignments

## First Category, then Faction

theta2.out.cat <- as.data.frame(round(mod.out.2$theta, 2))  ## round to two

colnames(theta2.out.cat) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(
                                 theta2.out.cat)))

theta2.out <- cbind(out$meta$pid, theta2.out.cat)

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

dim(paraTopicsK2) ## 5115 x 19

############################
## Identify the theorized shocks
## In the second theme (Programming)
############################

## Subset to Topic 1:
## Programs-dominant paragraphs:
programsParas <- paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="M2.Topic2"),]

dim(programsParas) ##3041x 31

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

### Model:
category.model= ~s(year)+ cat

set.seed(6889)

## Redistributor/Reciprocator
## Process-Category Covariate

mod.programs.themecat <- stm(documents=out$documents,
                        vocab=out$vocab,
                        data=out$meta,
                        K=10, ## 
                        prevalence=category.model,
                        content= ~cat,
                        seed=61921)

prep.programs.themecat <- estimateEffect(c(1:10) ~s(year)+ cat,
                                    mod.programs.themecat,
                                    metadata=out$meta,
                                    documents=out$documents,
                                    uncertainty=c("Global"))

summary(mod.programs.themecat)


## make.dt() combines doc-topic loadings (theta) +  metadata 

save.image(file="ProgramsSubSetstmYearStemmedCatRepl.Rdata")
