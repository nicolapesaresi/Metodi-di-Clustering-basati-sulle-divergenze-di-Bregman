set.seed(1)
source("bregmanhard.R")
library(ggplot2)

K1<-bregmanhard(iris[,-5],K=1)
K2<-bregmanhard(iris[,-5],K=2)
K3<-bregmanhard(iris[,-5],K=3)
K4<-bregmanhard(iris[,-5],K=4)
K5<-bregmanhard(iris[,-5],K=5)
K6<-bregmanhard(iris[,-5],K=6)

#gomito
V<-matrix(c(sum(K1$withins), sum(K2$withins), sum(K3$withins),
      sum(K4$withins), sum(K5$withins), sum(K6$withins),
      1,2,3,4,5,6),ncol=2,byrow=F)
       
ggplot(data.frame(V), aes(x = V[,2], y = V[,1])) +
  theme_light() +
  xlab("K") + ylab("Funzione di costo V") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_line() +
  geom_point()

#chalinski e harabasz
library(fpc)

CH2<-calinhara(iris[-5],K2$cluster)
CH3<-calinhara(iris[-5],K3$cluster)
CH4<-calinhara(iris[-5],K4$cluster)
CH5<-calinhara(iris[-5],K5$cluster)
CH6<-calinhara(iris[-5],K6$cluster)

CH<-matrix(c(CH2,CH3,CH4,CH5,CH6,2,3,4,5,6), ncol=2, byrow=F)

ggplot(data.frame(CH), aes(x = CH[,2], y = CH[,1])) +
  theme_light() +
  xlab("K") + ylab("Indice di Chalinski e Harabasz") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_line() +
  geom_point()

#hartigan
H2<-((sum(K2$withins)/sum(K3$withins))-1)/(nrow(iris[-5]-2-1))
H3<-((sum(K3$withins)/sum(K4$withins))-1)/(nrow(iris[-5]-2-1))
H4<-((sum(K4$withins)/sum(K5$withins))-1)/(nrow(iris[-5]-2-1))
H5<-((sum(K5$withins)/sum(K6$withins))-1)/(nrow(iris[-5]-2-1))
H6<-((sum(K6$withins)/sum(bregmanhard(iris[-5],K=7)$withins))-1)/(nrow(iris[-5]-2-1))

H<-matrix(c(H2,H3,H4,H5,H6,2,3,4,5,6), ncol=2, byrow=F)

ggplot(data.frame(H), aes(x = H[,2], y = H[,1])) +
  theme_light() +
  xlab("K") + ylab("Indice di Hartigan") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_line() +
  geom_point()

#silhouette
library(cluster)
D<-daisy(iris[-5])

s2<-mean(silhouette(K2$cluster,D)[,3])
s3<-mean(silhouette(K3$cluster,D)[,3])
s4<-mean(silhouette(K4$cluster,D)[,3])
s5<-mean(silhouette(K5$cluster,D)[,3])
s6<-mean(silhouette(K6$cluster,D)[,3])

S<-matrix(c(s2,s3,s4,s5,s6,2,3,4,5,6), ncol=2, byrow=F)
  
ggplot(data.frame(S), aes(x = S[,2], y = S[,1])) +
  theme_light() +
  xlab("K") + ylab("silhouette media") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_line() +
  geom_point()

#GAP statistic
GAP<-clusGap(iris[-5],bregmanhard,6,20)

G<-matrix(c(GAP$Tab[,3],1,2,3,4,5,6,GAP$Tab[,4]), ncol=3, byrow=F)

ggplot(data.frame(G), aes(x = G[,2], y = G[,1],ymin=G[,1]-G[,3],ymax=G[,1]+G[,3])) +
  theme_light() +
  xlab("K") + ylab("Gap statistic") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_line() +
  geom_point() +
  geom_errorbar(size=0.3, width=0.2)

