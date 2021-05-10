##library(stminsights)

rm(list=ls())

######################
## Declare Data Paths
######################

Sys.info()

if(Sys.info()['user']=="Ergane"){## desktop                              
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine                                              
    dataPathDesktop <- "../../../"
    print(paste0("On remote, data path is ", dataPathDesktop))
}

library(stm)

#####
## Summarize searchK for the full meeting range

## load(paste0(dataPathDesktop, "tradDevParaM1to113-Pandemic.RData"))

load(paste0(dataPathDesktop, "Pairwise/K24-25Model.Rdata")) ## for the model output
## load(paste0(dataPathDesktop, "stmM1to113ComparisonK20to25.Rdata")) ## looking for the metadata

ls()

meta <- out$meta

head(meta)

#####################
## Summarize output from models
#####################
## K=24 model: the model that we decided to move forward with

## K85 model: look for rates of pandemic
## topic

######################
### K= 24
#####################
attributes(mod.out.24)
summary(mod.out.24)

##Extract topics:

theta24Out <- as.data.frame(round(mod.out.24$theta, 3))  ## round to three places

dim(theta24Out) ## 8553x 24

head(theta24Out)
## replace "V-" in the column names
colnames(theta24Out) <- gsub(pattern="V",
                             replace="M24.Topic",
                             x=as.character(colnames(theta24Out)))
theta24Out <- cbind(meta$pid, theta24Out)

head(theta24Out)


##########
### Extract the highest proportion
## Attach to the paragraph IDs

#### Assigned paragraph topic in the 24 model:
theta24Out$max24 <- colnames(theta24Out[,2:22])[apply(
                                theta24Out[,2:22],1,which.max)] 

table(theta24Out$max24) ## There is some variation in the assignment of topics


## send out the paragraph-level topics:
## will add columns for each topic, which makes a bigger
## dataframe; but might save some hassle later

paraTopics <- merge(x =meta,
                    y=theta24Out,
                    by.x="pid",
                    by.y="meta$pid")

dim(paraTopics) ## 8853 x 41
colnames(paraTopics)

paraTopics$X <- NULL ## No idea what that column is or was

head(paraTopics)
tail(paraTopics)

summary(paraTopics$pid)

write.csv(paraTopics,
          file=paste0(dataPathDesktop, "M1to113DataAndTopics.csv"))

###########
## 24-topic model topic correlations

cors.24 <-  topicCorr(mod.out.24,
                      method = c("simple"),
                      cutoff = 0.2,
                      verbose = TRUE)


class(cors.24)
attributes(cors.24)

plot(cors.24,
     vertex.size=.75,
     vertex.color="darkblue")




