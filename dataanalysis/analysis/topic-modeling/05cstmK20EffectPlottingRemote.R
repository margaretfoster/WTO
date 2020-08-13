
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

###################################
## Load model
###################################
                
####################################
#### K20
####################################

load(paste0(dataPathDesktop, "tradDevPara_20Interact.RData"))

## mod.out.20 is the topic model results
## prep.20 is the estimated effects
## function is:
## s(numdate)*as.factor(income_level_iso3c)

plot(mod.out.20)

## How do the topics differ by income?




###################################
### Plotting over time
###################################

## Declare date ranges for the axes:

min(out$meta$date)
max(out$meta$date)

## Counter for months:
monthseq <- seq(from=min(out$meta$date),
                to=max(out$meta$date),
                by="month")
monthnames <- months(monthseq)

## counter for years
yearseq <- seq(from=min(out$meta$date),
                to=max(out$meta$date),
                by="year")
yearnames <- years(yearseq)

##############################
### Substantive questions:
############################
## What hapens with the small economies topic?

## Remind of income levels:
table(out$meta$income_level_iso3c)

dev.off()

##############
## Topic 5:
#############

plot(prep.20,
     topics=c(5),
     covariate="numdate",
     moderator="income_level_iso3c",
     moderator.value="LIC", ## This topic for Low income countries
     method="continuous",
     xlab=out$meta$date,
     xaxt="n"
     )

yearseq <- seq(from=min(out$meta$date),
                to=max(out$meta$date), by="year")
yearnames <- years(yearseq)
axis(1, at=as.numeric(yearseq)-min(as.numeric(yearseq)),
     labels=yearnames)


plot(prep.20,
      topics=c(5),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC",
      method="continuous",
     linecol="blue",
     add=TRUE,
     labels=FALSE,
      xaxt="n")
plot(prep.20,
      topics=c(5),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC",
      method="continuous",
     linecol="green",
     add=TRUE,
     xaxt="n",
     labels=FALSE)
plot(prep.20,
      topics=c(5),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="HIC",
      method="continuous",
     linecol="orange",
     add=TRUE,
     xaxt="n",
     labels=FALSE)


######################
## Topic 19 Market Access for special agreements
######################

incomeLevels <-c("LICs", "LMCs", "UMCs", "HICs")
incomecols <- c("red","orange", "green","blue")

dev.off()

plot(prep.20,
     topics=c(19),
     covariate="numdate",
     moderator="income_level_iso3c",
     moderator.value="LIC", ## This topic for Low income countries
     method="continuous",
     ylim=c(0, 1))

axis(1, at=as.numeric(monthseq)- min(as.numeric(monthseq)),
     labels=monthnames, las=2, cex=.5)

plot(prep.20,
      topics=c(19),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="LMC", #low middle income countries
      method="continuous",
     linecol="orange",
     add=TRUE,
     labels=FALSE,
     printlegend=FALSE)

plot(prep.20,
      topics=c(19),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC",
      method="continuous",
     linecol="green",
     add=TRUE,
     labels=FALSE,
     printlegend=FALSE)
plot(prep.20,
      topics=c(19),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="HIC",
      method="continuous",
     linecol="blue",
     add=TRUE,
          labels=FALSE,
     printlegend=FALSE)
legend("top",
       legend= incomeLevels,
       fill=incomcols, cex=.75)


###############################
## Topic 13: LDC access/dfqf
##############################

dev.off()

plot(prep.20,
     topics=c(13),
     covariate="numdate",
     moderator="income_level_iso3c",
     moderator.value="LIC", ## This topic for Low income countries
     method="continuous",
     ylim=c(0, 1))

axis(1, at=as.numeric(monthseq)- min(as.numeric(monthseq)),
     labels=monthnames, las=2, cex=.5)

plot(prep.20,
      topics=c(13),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC", #upper middle income countries
      method="continuous",
     linecol="orange",
     add=TRUE,
     printlegend=FALSE,
     labels=FALSE)
plot(prep.20,
      topics=c(13),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC",
      method="continuous",
     linecol="green",
     add=TRUE,
     printlegend=FALSE,
     labels=FALSE)
plot(prep.20,
      topics=c(13),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="HIC",
      method="continuous",
     linecol="blue",
     add=TRUE,
     printlegend=FALSE,
     labels=FALSE)


###############################
## Topic 20: "polite but negative"
##############################

dev.off()

plot(prep.20,
     topics=c(20),
     covariate="numdate",
     moderator="income_level_iso3c",
     moderator.value="LIC", ## This topic for Low income countries
     method="continuous",
     ylim=c(0, 1))

axis(1, at=as.numeric(monthseq)- min(as.numeric(monthseq)),
     labels=monthnames, las=2, cex=.5)

plot(prep.20,
      topics=c(20),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC", #upper middle income countries
      method="continuous",
     linecol="blue",
     add=TRUE,
     labels=FALSE)
plot(prep.20,
      topics=c(20),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC",
      method="continuous",
     linecol="green",
     add=TRUE,
     labels=FALSE)
plot(prep.20,
      topics=c(20),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="HIC",
      method="continuous",
     linecol="orange",
     add=TRUE,
     labels=FALSE)



###############################
## Topic 5: small and vulnerable
##############################

dev.off()

plot(prep.20,
     topics=c(5),
     covariate="numdate",
     moderator="income_level_iso3c",
     moderator.value="LIC", ## This topic for Low income countries
     method="continuous",
     ylim=c(0, 1))

axis(1, at=as.numeric(monthseq)- min(as.numeric(monthseq)),
     labels=monthnames, las=2, cex=.5)

plot(prep.20,
      topics=c(5),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC", #upper middle income countries
      method="continuous",
     linecol="orange",
     add=TRUE,
     printlegend=FALSE,
     labels=FALSE)
plot(prep.20,
      topics=c(5),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="UMC",
      method="continuous",
     linecol="green",
     add=TRUE,
     printlegend=FALSE,
     labels=FALSE)
plot(prep.20,
      topics=c(5),
      covariate="numdate",
      moderator="income_level_iso3c",
      moderator.value="HIC",
      method="continuous",
     linecol="blue",
     add=TRUE,
     printlegend=FALSE,
     labels=FALSE)
