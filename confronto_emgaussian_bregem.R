source("bregem.R")
source("emgaussian.R")
library(xtable)
set.seed(123)

#faithful
data("faithful")

muinit=list(faithful[2,],faithful[264,])

BregEM<-bregem(faithful,mu=muinit)
EM<-emgaussian(faithful,mu=muinit)

xtable(matrix(c(BregEM$mu[[1]],BregEM$mu[[2]]),ncol=2,byrow=T))
xtable(matrix(c(EM$mu[[1]],EM$mu[[2]]),ncol=2,byrow=T))

xtable(matrix(c(EM$mixprobs,EM$ll,EM$iter,
      BregEM$mixprobs,BregEM$ll,BregEM$iter),nrow=2,byrow=T))

library(ggplot2)
grsoft<-ggplot(faithful, aes(x = waiting, y = eruptions)) +
  theme_light() +
  xlab("waiting") + ylab("eruptions") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=BregEM$cluster+5)+
  ggtitle("Cluster bregem - Faithful - K = 2")

source("bregmanhard.R")
hard<-bregmanhard(faithful,nstart=5)

grhard<-ggplot(faithful, aes(x = waiting, y = eruptions)) +
  theme_light() +
  xlab("waiting") + ylab("eruptions") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=hard$cluster+5)+
  ggtitle("Cluster bregmanhard - Faithful - K = 2")

library(cowplot)
plot_grid(grsoft,grhard)

set.seed(12)

#artificiale
col1<-rnorm(150,mean=c(0,0.5,1),sd=1)
col2<-rnorm(150,mean=c(2,2.1,3),sd=1)
dati<-cbind(col1,col2)

muinit<-list(dati[1,],dati[2,],dati[3,])

BregEM<-bregem(dati,mu=muinit)
EM<-emgaussian(dati,mu=muinit)

xtable(matrix(c(BregEM$mu[[1]],BregEM$mu[[2]],BregEM$mu[[3]]),ncol=2,byrow=F))
xtable(matrix(c(EM$mu[[1]],EM$mu[[2]],EM$mu[[3]]),ncol=2,byrow=F))

xtable(matrix(c(EM$mixprobs,EM$ll,EM$iter,
                BregEM$mixprobs,BregEM$ll,BregEM$iter),nrow=2,byrow=T))


