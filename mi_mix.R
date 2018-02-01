mutinfo_mix <- function(X, Y, k = 3, direct=TRUE)
{
  #mutual information of X and Y 
  #Discrete-Continuous Mixtures
  if(is.vector(X)+is.vector(Y) < 2) stop("X and Y must be vectors.")
  
  n<- length(X)
  nx<- ny<- integer(n)
  p<- double(n)
  ki<- integer(n)
  ti<- double(n)
  
  for (i in 1:n){
    dx<- abs(X[i]- X)
    dy<- abs(Y[i]- Y)  
    d<- ifelse (dx > dy, dx, dy)
    
    p[i]<- max(d[rank(d,ties.method="min")<= (k+1)]) #exclude self-match
    
    if(p[i]==0){
      ki[i] <- length(which(d[-i]==0))
    }
    else{
      ki[i] <- k
    }
    nx[i]<- sum(dx[-i] <= p[i]) #exclude self-match
    ny[i]<- sum(dy[-i] <= p[i]) #exclude self-match
    ti[i]<- digamma(ki[i]) + log(n) - log(nx[i]+1) - log(ny[i]+1)
  }      
  
  mi <- mean(ti)
  return(mi)
}
