mutinfo.R<- function(X, Y, k = 3, direct=TRUE)
{
  #mutual information of X and Y 
  #KSG method by Kraskov, Stogbauer and Grassberger
  
  if(is.vector(X)+is.vector(Y) < 2) stop("X and Y must be vectors.")
  
  n<- length(X)
  
  nx<- ny<- integer(n)
  di<- double(n)
  
  for (i in 1:n){
    dx<- abs(X[i]- X)
    dy<- abs(Y[i]- Y)
    r<- ifelse (dx > dy, dx, dy)
    
    di[i]<- max(r[rank(r,ties.method="min")<= (k+1)])  #exclude self-match
    
    nx[i]<- sum(dx[-i] < di[i]) #exclude self-match
    ny[i]<- sum(dy[-i] < di[i]) #exclude self-match
  }      
  
  mi<- digamma(n) + digamma(k) - mean(digamma(nx+1) + digamma(ny+1))
  
  return(mi)
}
