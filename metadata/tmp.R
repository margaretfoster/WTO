## to visualize tsneout for only the first 1k words

library(ggplot2)

load("tsneout.rda")

ls()

class(tsneout)

dim(tsneout)

tsneout <- as.data.frame(tsneout)

colnames(tsneout) <- c("X", "Y")


gg <- ggplot(tsneout, aes(x=X, y=Y)) +  
    geom_point(size=0.25) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank()) +
    theme_bw()

gg
