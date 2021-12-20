## Helper function to callwordscores for a corpus

## takes a speaker-meeting level corpus
## returns a dataframe with wordscores attached

## 
library(quanteda)
library(quanteda.textmodels)

callWS <- function(corpus.subset,
                   fixed.point1="India",
                   fixed.point2="United States"){

    ## (0) Make corpus dfm
    corpus.dfm <- dfm(corpus.subset,
                      remove=stopwords("english"),
                      remove_punct = TRUE,
                      remove_numbers = TRUE)
    
    corpus.data <- as.data.frame(docvars(corpus.subset))
    
    ## (1) Set the reference countries and reference score matrix
    ## Default is India at -1 and US at 1
    first_index <- which(corpus.subset$country==fixed.point1)
    second_index <- which(corpus.subset$country==fixed.point2)
    
    refscores <- rep(NA, nrow(corpus.dfm))
    refscores[first_index] <- -1
    refscores[second_index] <- 1
    
    ## Fit the worscores model 
    
    ws.mod <- textmodel_wordscores(corpus.dfm,
                                   refscores,
                                   ##model="wordscores",
                                   scale="linear",
                                   smooth=1)
    
    scores<- predict(ws.mod,rescaling="mv")
    
    corpus.data$wordscore <- scores ## need to confirm indexing

    outlist <- list(wordscores=scores, data=corpus.data )
    
}
