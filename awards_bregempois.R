awards <- read.csv("~/Downloads/awards.txt", sep="")
awards$prog<-as.factor(awards$prog)
levels(awards$prog)<-c(1,2,3)
awards<-awards[-1]

set.seed(123)
source("bregempois.R")

#scelta K
K2<-bregempois(awards[-2],K=2)
K3<-bregempois(awards[-2],K=3)
K4<-bregempois(awards[-2],K=4)
K5<-bregempois(awards[-2],K=5)
K6<-bregempois(awards[-2],K=6)


library(fpc)
CH2<-calinhara(awards[-2],K2$cluster)
CH3<-calinhara(awards[-2],K3$cluster)
CH4<-calinhara(awards[-2],K4$cluster)
CH5<-calinhara(awards[-2],K5$cluster)
CH6<-calinhara(awards[-2],K6$cluster)

CH<-matrix(c(CH2,CH3,CH4,CH5,CH6,2,3,4,5,6), ncol=2, byrow=F)

library(ggplot2)

chalinski<-ggplot(data.frame(CH), aes(x = CH[,2], y = CH[,1])) +
  theme_light() +
  xlab("K") + ylab("Indice di Chalinski e Harabasz") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_line() +
  geom_point()

dataplot<-ggplot(awards, aes(x = num_awards, y = math) )+
  theme_light() +
  xlab("num_awards") + ylab("math") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point()

library(cowplot)
plot_grid(dataplot,chalinski)

mod<-K2

library(xtable)
xtable(rbind(mod$lambda[[1]],mod$lambda[[2]]))
xtable(matrix(c(mod$mixprobs,mod$ll,mod$iter),nrow=1))

graf<-ggplot(awards[-2], aes(x = num_awards, y = math)) +
  theme_light() +
  xlab("num_awards") + ylab("math") +
  theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=mod$cluster+4)

graf