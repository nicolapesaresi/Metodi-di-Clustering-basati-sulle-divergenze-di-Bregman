bregempois<-function(dati, lambda=NULL, mixprobs=NULL, K=2, hard=T, ciclimax=1000, sogliaconvergenza=0.00001)
{
  #inizializzazione
  stime<-list()
  dati<-data.matrix(dati)
  n<-nrow(dati)
  d<-ncol(dati)
  
  #imposto lambda se non e' dato
  if(is.null(lambda)) {lambda<-lapply(1:K,function(i) {rpois(ncol(dati),lambda=colMeans(dati))+0.1}) 
  #aggiungo 0.1 perchè lambda>0
  }else  { if(is.data.frame(lambda[[1]])) lambda<-lapply(1:K,function(i){as.numeric(lambda[[i]])})
  K<-length(lambda) }#imposto K se lambda e' dato
  #imposto mixprobs se non e' dato
  if(is.null(mixprobs)) mixprobs<-rep(1/K,K)
  
  #imposto primi valori di lambda e mixprobs
  stime[[1]]<-list(lambda=lambda, mixprobs=mixprobs)
  
  #funzione per calcolo densita poisson
  dens<-function(x, lamb, log=F){
    if(is.null(nrow(x)) || nrow(x)==1) x<-t(data.matrix(x)) #caso in cui x e' un vettore
    p<-rep(0,nrow(x))
    for (j in 1:nrow(x)) p[j]<-sum(exp(-lamb)*(lamb^x)/factorial(x)) #prob per ogni riga
    if(log==T) p<-log(p) #logprobs o probs
    return (p)
  } 
  
  #funzione per calcolo divergenza di Bregman
  #nel caso poisson è x*log(x/lambda)-(x-lambda)
  #siccome log(0)=-Inf, calcoliamo direttamente exp(-bregdiv)
  expbregdiv<-function(x, lamb){
    return(prod(((x/lamb)^x)*exp(lamb-x)))
  }
  
  #calcolo primo valore della funzione obiettivo
  stime[[1]]$fob = sum(log(rowSums(t(apply(dati,1,function(rigaj)
  {apply(matrix(1:K,ncol=1),1,function(i) 
  {stime[[1]]$mixprobs[i]*exp(-rigaj)*(stime[[1]]$lambda[[i]]^(rigaj)) * expbregdiv(rigaj,stime[[1]]$lambda[[i]]) /(stime[[1]]$lambda[[i]]*factorial(rigaj)) })})))))
  
  #loop
  c <- 2
  convergenza <- F
  
  while(convergenza==F & c<=ciclimax) {
    fob.old <- stime[[c-1]]$fob
    stime.old <- stime[[c-1]]
    
    #E step
    post.probs <- t(apply(dati,1,function(rigaj)
    {apply(matrix(1:K,ncol=1),1,function(i) {stime.old$mixprobs[i]*expbregdiv(rigaj,stime.old$lambda[[i]])})}))
    post.probs <- t(apply(post.probs, 1, function(x) x/sum(x))) #aggiungo denominatore
    
    # M step
    mixprobs.new <- apply(post.probs,2,mean) #aggiorno mixprobs
    lambda.new <- lapply(1:K, function(i)
    {denom <- n*mixprobs.new[i]
    return( as.vector(t(post.probs[,i])%*%dati)/denom)
    }) #aggiorno lambda
    
    stime[[c]] <- list(lambda=lambda.new, mixprobs=mixprobs.new)
    
    #aggiorno funzione obiettivo
    fob = sum(log(rowSums(t(apply(dati,1,function(rigaj)
    {apply(matrix(1:K,ncol=1),1,function(i) 
    {stime[[c]]$mixprobs[i]*exp(-rigaj)*(stime[[c]]$lambda[[i]]^(rigaj)) * expbregdiv(rigaj,stime[[c]]$lambda[[i]]) /(stime[[c]]$lambda[[i]]*factorial(rigaj)) })})))))
    
    stime[[c]]$fob<-fob
    stime[[c]]$iter<-c
    stime[[c]]$post.probs <- post.probs
    
    #verifico convergenza
    #------------------------
    if(abs(fob-fob.old) < sogliaconvergenza ) {convergenza<-T}
    c <- c+1 #aggiorno cicli
    
  }
  
  #calcolo log-verosimiglianza
  stime[[c-1]]$ll<-sum(log(rowSums(t(apply(dati,1,function(rigaj)
  {apply(matrix(1:K,ncol=1),1,function(i) 
  {stime[[c-1]]$mixprobs[i]*exp(-rigaj)*(stime[[c-1]]$lambda[[i]]^(rigaj))*expbregdiv(rigaj,stime.old$lambda[[i]])/(stime[[c-1]]$lambda[[i]]*factorial(rigaj))})})))))
  
  
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