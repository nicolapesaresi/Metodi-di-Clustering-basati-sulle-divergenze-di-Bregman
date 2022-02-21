source("bregmanhard.R")
set.seed(1)

#nstart 3
nstart=3

#random
for (q in 1:nstart){
  randomcluster<-sample(c(1,2,3),size=150,replace=T)
  murandom<-matrix(c(colMeans(iris[which(randomcluster==1),-5]),
                     colMeans(iris[which(randomcluster==2),-5]),
                     colMeans(iris[which(randomcluster==3),-5])),ncol=4,byrow=T)
  
  mod<-bregmanhard(iris[-5],centroidi=murandom)
  
  if(q==1) {random<-mod
  var<-sum(mod$withins)
  }else {if (sum(mod$withins)<var){random<-mod
  var<-sum(random$withins)}}
}

#forgy
forgy<-bregmanhard(iris[-5],K=3,nstart=3)

#MacQueen
for (q in 1:nstart){
  clusters<-rep(0,150)
  mumacqueen<-iris[sample(1:150,size=3),-5]
  for (j in 1:150){
    distanze<-rep(0,3) #inizializzo vettore distanze per oss j
    for (i in 1:3){ #calcolo distanza dell'oss j da ogni centroide
      d<-as.double(dist(rbind(iris[j,-5],mumacqueen[i,])))
      distanze[i]<-d
    }
    c<-which.min(distanze)
    clusters[j]<-c #assegno oss j a cluster con minor distanza
    mumacqueen[c,]<-colMeans(iris[which(clusters==c),-5]) }
  
  mod<-bregmanhard(iris[-5],centroidi=mumacqueen)
  
  if(q==1) {macqueen<-mod
  var<-sum(mod$withins)
  }else {if (sum(mod$withins)<var){macqueen<-mod
  var<-sum(macqueen$withins)}}
}

#kaufman
library(pracma)#per mediana geometrica
for (q in 1:nstart){
  mukaufman=matrix(NA,nrow=3,ncol=4)
  mukaufman[1,]<-geo_median(as.matrix(iris[-5]))$p
  
  for(h in 2:3){
    for (j in 1:150){
      distanzecentr<-rep(0,3)
      distanzeoss<-rep(0,150)
      for (i in 1:3){ #calcolo distanza dell'oss j da ogni centroide
        distanzecentr[i]<-as.double(dist(rbind(iris[j,-5],mukaufman[i,])))
      }
      D<-distanzecentr[which.min(distanzecentr)]
      for (z in 1:150){
        d<-as.double(dist(rbind(iris[j,-5],iris[z,-5])))
        distanzeoss[j]<-distanzeoss[j]+max(D-d,0)
      }
    }
    mukaufman[h,]<-as.matrix(iris[which.max(distanzeoss),-5])
  }
  
  
  
  mod<-bregmanhard(iris[-5],centroidi=mukaufman)
  
  if(q==1) {kaufman<-mod
  var<-sum(mod$withins)
  }else {if (sum(mod$withins)<var){kaufman<-mod
  var<-sum(kaufman$withins)}}
}

#inizializzazione kmeans++
for (q in 1:nstart){
  mukmpp=matrix(NA,nrow=3,ncol=4)
  mukmpp[1,]<-as.matrix(iris[sample(1:150,1),-5])
  
  for (h in 2:3){
    centrprobs<-rep(0,150)
    for (j in 1:150){
      distanze<-rep(0,3) #inizializzo vettore distanze per oss j
      for (i in 1:3){ #calcolo distanza dell'oss j da ogni centroide
        d<-as.double(dist(rbind(iris[j,-5],mukmpp[i,])))
        distanze[i]<-d
      }
      D<-distanze[which.min(distanze)]^2
      centrprobs[j]<-D
    }
    centrprobs<-centrprobs/sum(centrprobs)
    mukmpp[h,]<-as.matrix(iris[sample(1:150,1,prob=centrprobs),-5])
  }
  
  
  mod<-bregmanhard(iris[-5],centroidi=mukmpp)
  
  if(q==1) {kmpp<-mod
  var<-sum(mod$withins)
  }else {if (sum(mod$withins)<var){kmpp<-mod
  var<-sum(kmpp$withins)}}
}

#confronto
library(ggplot2)
reali<-iris
levels(reali$Species)<-c(2,3,4)

realclust<-ggplot(iris[c(1,3,5)], aes(x = Sepal.Length, y = Petal.Length)) +
  theme_light() +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=reali$Species)+
  ggtitle("Cluster reali")

random$cluster<-(as.factor(random$cluster))
levels(random$cluster)<-c(3,4,2) #per colori

randomclust<-ggplot(iris[c(1,3)], aes(x = Sepal.Length, y = Petal.Length)) +
  theme_light() +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=random$cluster)+
  ggtitle("Inizializzazione Random")


forgy$cluster<-(as.factor(forgy$cluster))
levels(forgy$cluster)<-c(3,4,2) #per colori

forgyclust<-ggplot(iris[c(1,3)], aes(x = Sepal.Length, y = Petal.Length)) +
  theme_light() +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=forgy$cluster)+
  ggtitle("Inizializzazione Forgy")

macqueen$cluster<-(as.factor(macqueen$cluster))
levels(macqueen$cluster)<-c(3,4,2) #per colori

macqueenclust<-ggplot(iris[c(1,3)], aes(x = Sepal.Length, y = Petal.Length)) +
  theme_light() +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=macqueen$cluster)+
  ggtitle("Inizializzazione MacQueen")

kaufman$cluster<-(as.factor(kaufman$cluster))
levels(kaufman$cluster)<-c(3,4,2) #per colori

kaufmanclust<-ggplot(iris[c(1,3)], aes(x = Sepal.Length, y = Petal.Length)) +
  theme_light() +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=kaufman$cluster)+
  ggtitle("Inizializzazione Kaufman")


kmpp$cluster<-(as.factor(kmpp$cluster))
levels(kmpp$cluster)<-c(3,2,4) #per colori

kmppclust<-ggplot(iris[c(1,3)], aes(x = Sepal.Length, y = Petal.Length)) +
  theme_light() +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=kmpp$cluster)+
  ggtitle("Inizializzazione K-Means++")


library(cowplot)
plot_grid(realclust,randomclust,forgyclust,macqueenclust,kaufmanclust,kmppclust)

matr<-matrix(c(round(table(random$cluster)/sum(table(random$cluster)),3),round(sum(random$withins),3),round(random$iter,0), round(length(which(random$cluster==reali$Species))/150,3),
               round(table(forgy$cluster)/sum(table(forgy$cluster)),3),round(sum(forgy$withins),3),round(forgy$iter,0), round(length(which(forgy$cluster==reali$Species))/150,3),
               round(table(macqueen$cluster)/sum(table(macqueen$cluster)),3),round(sum(macqueen$withins),3),round(macqueen$iter,0),round(length(which(macqueen$cluster==reali$Species))/150,3),
               round(table(kaufman$cluster)/sum(table(kaufman$cluster)),3),round(sum(kaufman$withins),3),round(kaufman$iter,0),round(length(which(kaufman$cluster==reali$Species))/150,3),
               round(table(kmpp$cluster)/sum(table(kmpp$cluster)),3),round(sum(kmpp$withins),3),round(kmpp$iter,0),round(length(which(kmpp$cluster==reali$Species))/150,3),
               round(table(reali$Species)/sum(table(reali$Species)),3),NA,NA,NA
),ncol = 6,byrow=T)

library(xtable)
xtable(matr)
