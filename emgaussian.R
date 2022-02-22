emgaussian<-function(dati, mu=NULL, mixprobs=NULL, K=2, hard=T, ciclimax=1000, sogliaconvergenza=0.00001)
{
  #inizializzazione
  stime<-list()
  dati<-data.matrix(dati)
  n<-nrow(dati)
  d<-ncol(dati)
  
  #imposto mu se non e' dato
  if(is.null(mu)) {mu<-lapply(1:K,function(i){rnorm(ncol(dati),mean=colMeans(dati),sd=1)})
  }else  { K<-length(mu)
            if(is.data.frame(mu[[1]])) mu<-lapply(1:K,function(i){as.numeric(mu[[i]])})
          }#imposto K se mu e' dato
  #imposto mixprobs se non e' dato
  if(is.null(mixprobs)) mixprobs<-rep(1/K,K)
  
  #imposto primi valori di mu e mixprobs
  stime[[1]]<-list(mu=mu, mixprobs=mixprobs)
  
  #funzione per calcolo densita normale
  dens<-function(x, media, log=F){
    if(is.null(nrow(x)) || nrow(x)==1) {x <- t(data.matrix(x)) } #caso in cui x e' un vettore
    dis<-rep(0,nrow(x))
    for (j in 1:nrow(x)) { dis[j] <- (t(x[j,]-media)) %*% (x[j,]-media) } #distanza per ogni riga
    r<-(-0.5)*((dis)+(ncol(x)*log(2*pi))) 
    if(log==F) r<-exp(r) #logprobs o probs
    return (r)
  } 
  
  #calcolo primo valore della log-verosimiglianza
  stime[[1]]$ll = sum(log(rowSums(t(apply(dati,1,function(rigaj)
  {apply(matrix(1:K,ncol=1),1,function(i) 
  {stime[[1]]$mixprobs[i]*dens(rigaj,stime[[1]]$mu[[i]])})})))))
  
  #loop
  c <- 2
  convergenza <- F
  
  while(convergenza==F & c<=ciclimax) {
    ll.old <- stime[[c-1]]$ll
    stime.old <- stime[[c-1]]
    
    #E step
    post.probs <- t(apply(dati,1,function(rigaj)
    {apply(matrix(1:K,ncol=1),1,function(i) {stime.old$mixprobs[i]*dens(rigaj,stime.old$mu[[i]])})}))
    post.probs <- t(apply(post.probs, 1, function(x) x/sum(x))) #aggiungo denominatore
    
    # M step
    mixprobs.new <- apply(post.probs,2,mean) #aggiorno mixprobs
    mu.new <- lapply(1:K, function(i)
    {denom <- n*mixprobs.new[i]
    return( as.vector(t(post.probs[,i])%*%dati)/denom)
    }) #aggiorno mu
    
    stime[[c]] <- list(mu=mu.new, mixprobs=mixprobs.new)
    
    #aggiorno log-verosimiglianza
    ll <- sum(log(rowSums(t(apply(dati,1,function(rigaj)
    {apply(matrix(1:K,ncol=1),1,function(i) 
    {stime[[c]]$mixprobs[i]*dens(rigaj,stime[[c]]$mu[[i]])})})))))
    
    
    stime[[c]]$ll<-ll
    stime[[c]]$iter<-c
    stime[[c]]$post.probs <- post.probs
    
    #verifico convergenza
    #------------------------
    if(abs(ll-ll.old) < sogliaconvergenza ) {convergenza<-T}
    c <- c+1 #aggiorno cicli
  } 
  
  #se l'opzione hard è impostata come T, passo da soft a hard clustering
  #assegnando le osservazioni al cluster che ha posterior probability maggiore
  if (hard==T){
    cluster<-rep(0,nrow(stime[[c-1]]$post.probs))
    for (j in 1:nrow(stime[[c-1]]$post.probs)){
      cluster[j]<-which.max(stime[[c-1]]$post.probs[j,])
    }
  }
  else cluster<-NULL
  stime[[c-1]]$cluster<-cluster
  
  
  return(stime[[c-1]])
}
