## Checking to see if I run the get sentences +
## sentiment analyzer in one script
## if that will maintain the indexing between
## get sentiments and the sentence-level dataframe
## (without saving and reloading the data)


library(sentimentr)

data <- read.csv("WTO_TD_NER_Data.csv")


dim(data)
colnames(data)

data$X <- NULL
data$date <- as.Date(data$date)

summary(data$date)

data.sent <- get_sentences(data)

dim(data.sent)
colnames(data.sent)

data.sent[4726:4730, c("paranum","paratext",
                     "date", "element_id",
                     "sentence_id" )]


data.sent[3726:3730, c("paranum","paratext",
                     "date", "element_id",
                     "sentence_id" )]


## naive analyizer:

sentiments <- sentiment(data.sent,
                        n.before=Inf,
                        n.after=Inf)

## pull a subset to compare expected direction + magnitude
## and the SentimentR results:

colnames(sentiments)

data.sent[1639: 1642, c("paranum", "paratext",
                         "element_id", "sentence_id")]


sentiments[15890: 15895, c("paranum", "paratext", "sentiment",
                         "element_id", "sentence_id",
                         "firstent")]


head(lexicon::hash_sentiment_jockers_rinker)
df <- lexicon::hash_sentiment_jockers_rinker

dim(df) ##11710

summary(lexicon::hash_sentiment_jockers_rinker$y) #-2 to 1

lexicon::hash_sentiment_jockers_rinker[which(lexicon::hash_sentiment_jockers_rinker$y==-2.00),]
