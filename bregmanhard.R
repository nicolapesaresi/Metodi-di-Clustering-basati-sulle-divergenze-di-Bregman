bregmanhard<-function(dati,K=2,centroidi=NULL,nstart=1,ciclimax=10)
{
  #inizializzazione
  if(is.null(centroidi)){centroidi=dati[sample(1:nrow(dati),size=K),]} #inizializzazione forgy
  else {K<-nrow(centroidi)}
  
  risultati<-list()
  modvar<-rep(0,nstart)
  
  for (q in 1:nstart){
    if (q!=1) {centroidi<-dati[sample(1:nrow(dati),size=K),]} #provo diversi centroidi a ogni iterazione
    
    clusters<-rep(0,nrow(dati))
    clusters.old<-clusters
    cicli<-0
    convergenza<- F
    
    while(cicli<ciclimax & convergenza==F){
      #assegnazione
      for (j in 1:nrow(dati)){
        distanze<-rep(0,K) #inizializzo vettore distanze per oss j
        for (i in 1:K){ #calcolo distanza dell'oss j da ogni centroide
          d<-as.double(dist(rbind(dati[j,],centroidi[i,])))
          distanze[i]<-d
        }
        clusters[j]<-which.min(distanze) #assegno oss j a cluster con minor distanza
      }
      
      #aggiornamento
      for (i in 1:K) {centroidi[i,]<-apply(dati[which(clusters==i),],MARGIN=2,FUN=mean) }  #nuovi centroidi
      cicli<-cicli+1 #conto iterazioni
      
      #convergenza
      if (isTRUE(all.equal(clusters,clusters.old))) { convergenza<-T }
      clusters.old<-clusters
    }
    
    #calcolo bonta cluster come somma delle varianze within (tra gruppi)
    withins<-rep(0,K)
    for (i in 1:K){
      for (j in which(clusters==i)){
        d<-(dist(rbind(dati[j,],centroidi[i,])))
        withins[i]<-withins[i]+d^2
      }
    }

    #aggiungo a lista risultati
    risultati[[q]]<-list(cluster=clusters,centers=centroidi,withins=withins,iter=cicli)
    modvar[q]<-sum(withins)
  }
  
  #seleziono miglior modello
  mod<-which.min(modvar)
  return(risultati[[mod]])
  #print(risultati[[mod]])
}