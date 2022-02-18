set.seed(123)
library(ggplot2)

col1<-c(rnorm(50,5,1),rnorm(200,10,3),rnorm(50,15,1))
col2<-c(rnorm(50,5,1),rnorm(200,10,3),rnorm(50,15,1))
col3<-c(rep(2,50),rep(3,200),rep(1,50))

tab<-as.data.frame(cbind(col1,col2,col3))

gr1<-ggplot(tab, aes(x = tab[,1], y = tab[,2])) +
  theme_light() +
  xlab("x") + ylab("y") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=tab[,3]+1)+
  ggtitle("Cluster Reali")

tab2<-tab
tab2[,3]<-kmeans(tab[,-3],3)$cluster

gr2<-ggplot(tab2, aes(x = tab2[,1], y = tab2[,2])) +
  theme_light() +
  xlab("x") + ylab("y") +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank(),panel.grid.major.y = element_blank() ) +
  geom_point(color=tab2[,3]+1) +
  ggtitle("Cluster K-Means")

library(cowplot)
plot_grid(gr1,gr2)