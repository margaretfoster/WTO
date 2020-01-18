##non-network figures for MPSA presentation

library(ggplot2)
library(RColorBrewer)
library(stm)

data <-read.csv("~/Dropbox/WTO/data/nodedeltas_all.csv")


data$name <- as.character(data$name)

## NER data-- want a plot with
## bars for year on y axis, and paragraph number
## colored by speaker if in big 5 or 'other' if not

paradata <- read.csv("~/Dropbox/WTO/data/NER/WTODataNew.csv",
                     header=FALSE, stringsAsFactors=FALSE)

dim(paradata)
colnames(paradata) ## v3= paragraph

## subset: who speaks in what order:
speakers <- paradata[c("V2",'V3', 'V5', 'V6')]

colnames(speakers) <- c('meeting','paragraph', 'speaker', 'date')
speakers$date <- as.Date(speakers$date, format="%m/%d/%y")
speakers$year <- format(as.Date(speakers$date, format="%d/%m/%Y"),"%Y")
speakers$month <- format(as.Date(speakers$date, format="%d/%m/%Y"),"%m")

## goal: sort by date, then paragraph number, then add a counter
## counter restarts at year breaks

spk <- data.frame()
for(m in unique(speakers$meeting)){
    dt <- speakers[which(speakers$meeting==m),]
    dt <- dt[order(dt$date, dt$paragraph),]
    dt$counter <- 1:dim(dt)[1]
    spk <- rbind(spk, dt)
}

## frequent speakers over time;

freqs <- as.data.frame(sort(table(spk$speaker),
                            decreasing=TRUE))

freqs[1:15,]
## 10 most common speakers:
mostcommon <- as.character(freqs$Var1[1:12])

big5 <- c("Brazil", "China", "India", "Egypt",
          "European Communities", "European Union", "United States")

brewer.pal(n = 7, name = "Spectral")

## simplify the speakers:
## keep name if in most common list
spk$simple <- ifelse(spk$speaker %in% mostcommon,
                     spk$speaker, "Other")

spk$big5 <- ifelse(spk$speaker %in% big5,
                   spk$speaker, "Other")

## graph;
## colors with grey as 7 of 8:
cols <- c("#D53E4F", "#FC8D59", "#FEE08B",
          "#FFFFBF", "#E6F598", "#99D594",
          "grey", "#3288BD")

pdf(file="meetingseqeuence.pdf")
ss <- ggplot(spk, aes(x=meeting, y=counter)) +
    geom_tile(aes(fill = big5)) +
    theme_bw()
##ss <- ss +facet_wrap(~year, nrow=4)
ss <- ss + scale_fill_manual(values=cols)
ss <- ss + ylab("Speaker Turn") + xlab("Meeting")
ss <- ss + labs(fill="Representative from:")
ss <- ss + theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())
##ss <- ss + theme(axis.text.x=element_text(angle=90, hjust=1))
ss
dev.off() 


###############################
###Country-level:
################################

## What happens to Venezuela over time:
## Do they become a pariah?


vz <- data[which(data$name=="Venezuela"),]

vz[order(vz$y),]

pdf(file="VenezuelaActivity.pdf")
vv<- ggplot(data=vz,
             aes(y=indeg,
                 x=y))
vv <- vv+ geom_line(color="lightskyblue") + theme_bw()
vv <- vv+ geom_line(data=vz,
                     aes(y=outdeg,
                         x=y),
                    color="darkred")

vv <-  vv+  xlab("Years") + ylab("In and Out Degree Activity") +
    ggtitle("Reference Activity for Venezuela\n In (blue) and Out (red) Degree")
vv
dev.off()

### Japan

jpn <- data[which(data$name=="Japan"),]

jpn[order(jpn$y),]

pdf(file="JapanActivity.pdf")
vv<- ggplot(data=jpn,
             aes(y=indeg,
                 x=y))
vv <- vv+ geom_line(color="lightskyblue") + theme_bw()
vv <- vv+ geom_line(data=jpn,
                     aes(y=outdeg,
                         x=y),
                    color="darkred")

vv <-  vv+  xlab("Years") + ylab("In and Out Degree Activity") +
    ggtitle("Reference Activity for Japan\n In (blue) and Out (red) Degree")
vv

dev.off()


### Canada

cdn <- data[which(data$name=="Canada"),]

cdn[order(cdn$y),]

pdf(file="CanadaActivity.pdf")
vv<- ggplot(data=cdn,
             aes(y=indeg,
                 x=y))
vv <- vv+ geom_line(color="lightskyblue") + theme_bw()
vv <- vv+ geom_line(data=cdn,
                     aes(y=outdeg,
                         x=y),
                    color="darkred")

vv <-  vv+  xlab("Years") + ylab("In and Out Degree Activity") +
    ggtitle("Paragraph Activity for Canada\n In (blue) and Out (red) Degree")
vv
dev.off()

### switzerland


ch <- data[which(data$name=="Switzerland"),]

ch[order(ch$y),]

pdf(file="SwitzerlandActivity.pdf")
vv<- ggplot(data=ch,
             aes(y=indeg,
                 x=y))
vv <- vv+ geom_line(color="lightskyblue") + theme_bw()
vv <- vv+ geom_line(data=ch,
                     aes(y=outdeg,
                         x=y),
                    color="darkred")

vv <-  vv+  xlab("Years") + ylab("In and Out Degree Activity") +
    ggtitle("Paragraph Activity for Switzerland\n In (blue) and Out (red) Degree")
vv
dev.off()


##


## who has largest # of others referencing them
## relative to their own references?

data[order(data$delta),][1:50,]
head(data)

unique(data$name)

### large asymmetries
ofinterest <- data[grep("China|UnitedStates|India|CostaRica|Brazil|Kenya",
                   data$name),]
dim(ofinterest)

gg <- ggplot(data=ofinterest,
             aes(y=delta,
                 x=y,
                  color=as.factor(ofinterest$name)))
gg <- gg+ geom_point() + theme_bw()

gg


ofinterest2 <- data[grep("China|UnitedStates|India",data$name),]

dim(ofinterest2)

pdf(file="senderrec.pdf")
gg <- ggplot(data=ofinterest2,
             aes(y=delta,
                 x=y,
                 color=as.factor(ofinterest2$name)))
gg <- gg+ geom_line() + theme_bw()
gg <- gg + geom_line(y=0, color="red", linetype=3)
gg <- gg+  xlab("Years") + ylab("Out Degree - In Degree") +
    ggtitle("Referer Deltas") + labs(colour = "Countries")
gg

dev.off()


## Disproportionate senders/ recievers 2001

sr2001 <- data[which(data$y==2001),]

dim(sr2001)

## disproportional recievers
sr2001[order(sr2001$delta), ][1:10,]
## disproportional senders
sr2001[order(sr2001$delta), ][72:82,]

## most active senders
sr2001[order(sr2001$outdeg), ][72:82,]

## most active recievers
sr2001[order(sr2001$indeg), ][72:82,]


##
## Disproportionate senders/ recievers 2002

sr2002 <- data[which(data$y==2002),]

dim(sr2002) ##86x7
 
## disproportional recievers
sr2002[order(sr2002$delta), ][1:10,]
## disproportional senders
sr2002[order(sr2002$delta), ][76:86,]


## most active senders
sr2002[order(sr2002$outdeg), ][76:86,]

## most active recievers
sr2002[order(sr2002$indeg), ][76:86,]

## Disproportionate senders/ recievers 2008
## after financial crisis

sr2008 <- data[which(data$y==2008),]

dim(sr2008) ##60x7
 
## disproportional recievers
sr2008[order(sr2008$delta), ][1:10,]
## disproportional senders
sr2008[order(sr2008$delta), ][50:60,]


## most active senders
sr2008[order(sr2008$outdeg), ][50:60,]

## most active recievers
sr2008[order(sr2008$indeg), ][50:60,]

## Disproportionate senders/ recievers 2009
## after financial crisis

sr2008 <- data[which(data$y==2008),]

dim(sr2008) ##60x7
 
## disproportional recievers
sr2008[order(sr2008$delta), ][1:10,]
## disproportional senders
sr2008[order(sr2008$delta), ][50:60,]


## most active senders
sr2008[order(sr2008$outdeg), ][50:60,]

## most active recievers
sr2008[order(sr2008$indeg), ][50:60,]


ls()

## what meetings do we have basically no data for:
colnames(paradata)## V2 is meeting name

lts <- as.data.frame(summary(as.factor(paradata$V2)))




