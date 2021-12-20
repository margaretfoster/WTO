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

load("twoTopicsAndSubSets-NoAdminSubset.Rdata")

############################
##### Analysis
############################
## Extract paragraph-level topic assginments

theta2.out <- as.data.frame(round(mod.out.2$theta, 2))  ## round to two
colnames(theta2.out) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(theta2.out)))
theta2.out <- cbind(out$meta$pid, theta2.out)

#### Assign dominant topic
theta2.out$assignedtopic <- colnames(theta2.out[,2:3])[apply(
                                theta2.out[,2:3],1,which.max)] 

table(theta2.out$assignedtopic)


head(theta2.out)

paraTopicsK2 <- merge(x =out$meta,
                    y=theta2.out,
                    by.x="pid",
                    by.y="out$meta$pid")
dim(paraTopicsK2) ## 5227 x 27

colnames(paraTopicsK2)

## Subset to each domain:

## Development -dominant paragraphss
devParas <- paraTopicsK2[which(paraTopicsK2$assignedtopic=="M2.Topic1"),]

dim(devParas) ## 1879 x 19

colnames(devParas)
devSpeakers <- as.data.frame(table(devParas$firstent))
devSpeakers <- devSpeakers[order(devSpeakers$Freq),]
tail(devSpeakers)## Top speakers are US (381); EU (203);
## India (192); Egypt (160); China (120); Brazil (80)


### Iterate the topic model on subsets
## Subset One: development paragraphs

devParas$paratext <- gsub("[[:digit:]]", "",
                 devParas$paratext)

devParas$paratext <- gsub("[[:punct:]]", "",
                 devParas$paratext)


data2 <- corpus(devParas, text_field = 'paratext')
docvars(data2)$text <- as.character(data2)

dat2 <- dfm(data2, stem = TRUE,
            remove = stopwords('english'),
            remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 2)

out <- convert(dat2, to = 'stm')

length(out)

form= ~s(numdate)

mod.out.dev <- stm(documents=out$documents,
                  vocab=out$vocab,
                  data=out$meta,
                 K=5, ## 
                  prevalence=form ,
               seed=61921)
 
prep.dev <- estimateEffect(c(1:5) ~s(numdate),
                         mod.out.dev,
                          metadata=out$meta,
                          documents=out$documents,
                          uncertainty=c("Global"))


summary(mod.out.dev)


mod2.out <- as.data.frame(round(mod.out.dev$theta, 2))  ## round to two

head(mod2.out)

colnames(mod2.out) <- gsub(pattern="V",
                           replace="M2.Topic",
                           x=as.character(colnames(mod2.out)))

head(mod2.out)

mod2.out <- cbind(out$meta$pid, mod2.out)

head(mod2.out)

dim(mod2.out)

#### Assign dominant topic

mod2.out$assignedtopic <- colnames(mod2.out[,2:6])[apply(
                                mod2.out[,2:6 ],1,which.max)]  

table(mod2.out$assignedtopic)

write.csv(mod2.out,
          file="withinDevTheme.csv")

save.image(file="themeSubsetDevelopment.Rdata")
