source("bregem.R")
library(mclust)
library(ggplot2)
set.seed(123)

col1<-rnorm(1000,mean=c(1,2),sd=1)
col2<-rnorm(1000,mean=c(6,8),sd=1)
dati<-cbind(col1,col2)
col3<-rep(c(1,2),500) #cluster reali

muinit<-list(dati[1,],dati[2,])

BregEM<-bregem(dati,mu=muinit)
mc<-Mclust(dati,G=2,modelNames="EII",subset=muinit)

xtable(matrix(c(BregEM$mu[[1]],BregEM$mu[[2]]),nrow=2,byrow=T))
xtable(matrix(c(mc$parameters$mean[,1],mc$parameters$mean[,2]),nrow=2,byrow=T))
xtable(matrix(c(BregEM$mixprobs,BregEM$ll,
                mc$parameters$pro,mc$loglik),nrow=2,byrow=T))

grafreal<-ggplot(data.frame(dati), aes(x = col1, y = col2) )+
  theme_light() +
  xlab("X1") + ylab("X2") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=col3)

grafbreg<-ggplot(data.frame(dati), aes(x = col1, y = col2) )+
  theme_light() +
  xlab("X1") + ylab("X2") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=BregEM$cluster)

grafmclust<-ggplot(data.frame(dati), aes(x = col1, y = col2) )+
  theme_light() +
  xlab("X1") + ylab("X2") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=mc$classification)

library(cowplot)
plot_grid(grafreal,grafbreg,grafmclust, nrow=1)
