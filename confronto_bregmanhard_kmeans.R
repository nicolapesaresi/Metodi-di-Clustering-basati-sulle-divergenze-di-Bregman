source("bregmanhard.R")
library(xtable)

set.seed(123)
#inizializzazione 
centr<-iris[c(1,51,101),-5]

#confronto con k-means
breg<-bregmanhard(iris[,-5],centroidi=centr,ciclimax=10)
KM<-kmeans(iris[,-5],centers=centr,algorithm = "Lloyd")

xtable(breg$centers)
xtable(KM$centers)
xtable(rbind(breg$withins,KM$withinss))
