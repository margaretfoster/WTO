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

#####################################
##### Subset: Topic Two-- Process Frame
######################################

summary(mod.out.2) ## 

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

prop.table(table(theta2.out$assignedtopic))

head(theta2.out)

paraTopicsK2 <- merge(x =out$meta,
                    y=theta2.out,
                    by.x="pid",
                    by.y="out$meta$pid")

dim(paraTopicsK2) ## 5228 x 19

colnames(paraTopicsK2)

head(paraTopicsK2)

############################
## Identify the theorized shocks
## In the process group
############################

colnames(out$meta)
## Subset to each domain:

## Process dominant paragraphs:
processParas <- paraTopicsK2[which(paraTopicsK2$assignedtopic=="M2.Topic2"),]

dim(processParas) ##3350 x 23

processSpeakers <- as.data.frame(
    table(processParas$firstent))
processSpeakers <- processSpeakers[order(
    processSpeakers$Freq),]
tail(processSpeakers)## Top speakers are:
## US (429); EU (248);
## India (229); Egypt (173);
## China (136), and Brazil (89)

### Iterate the topic model on subsets
## and add the shocks:

## Subset One: processelopment paragrahs

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

length(out)
attributes(out)

colnames(out$meta)

round(prop.table(table(out$meta$"income_level_iso3c")), 2)

form.base= ~s(numdate)


set.seed(6889)
#### Theme Cluster 2 Models:

mod.out.theme2 <- stm(documents=out$documents,
                  vocab=out$vocab,
                  data=out$meta,
                 K=5, ## 
                  prevalence=form ,
               seed=61921)
 
prep.out.theme2 <- estimateEffect(c(1:5) ~s(numdate),
                         mod.out.theme2,
                          metadata=out$meta,
                          documents=out$documents,
                          uncertainty=c("Global"))


summary(mod.out.theme2)
## Topic 1: Commodities from LDCs
## Topic 2: LDC Market Access Provisions
## Topic 3: E-Commerce
## Topic 4: Technical Assistance
## Topic 5: Debates

topicNames <- c("Commodities",
                "LDC Market Access",
                "Ecommerce",
                "Technical Assistance",
                "Policy Debates")

pdf(file="Theme2Overall.pdf")
par(bty="n",col="grey40",lwd=5)
plot.STM(mod.out.theme2,
         type="summary",
         custom.labels="",
         topic.names=topicNames)

dev.off()

plot(prep.out.theme2,
     covariate="numdate",
     method = "continuous",
     topics = c(1:5),
     model = mod.out.theme2,
     printlegend = TRUE,
     xaxt = "n",
     xlab = "Time (1995-2020)")

monthseq <- seq(from = as.Date("1995-04-04"),
                to = as.Date("2020-11-20"),
                by = "month")
monthnames <- months(monthseq)

axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)), labels = monthnames)

dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%
## Ascribe topics to paragraphs

mod2.out <- as.data.frame(round(mod.out.theme2$theta, 2))  ## round to two                                                      

head(mod2.out)

colnames(mod2.out) <- gsub(pattern="V",
                           replace="MT2.Topic",
                           x=as.character(colnames(mod2.out)))

head(mod2.out)

mod2.out <- cbind(out$meta$pid, mod2.out)

head(mod2.out)

mod2.out$assignedtopic <- colnames(mod2.out[,2:6])[apply(
                                mod2.out[,2:6 ],1,which.max)]

table(mod2.out$assignedtopic)

write.csv(mod2.out,
          file="withinTheme2.csv")

### Plots
## First: Plot all over time:


ls()
save.image(file="themeSubsetTheme2.Rdata")

