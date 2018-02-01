fset <- function(MI,X,Y,m){
  # MI: function of estimating mutual information (KDE_MI,mutinfo.R,mutinfo_mix)
  # m: the number of features
  n <- ncol(X)
  x_new <- c(1:n)
  x_s <- c()
  while(length(x_s)< m){
    P = rep(NA,n)
    D = rep(NA,n)
    R = rep(NA,n)
    
    for(i in x_new){
      D[i] <- MI(X[[i]],Y)
      if(length(x_s)==0){
        P[i] <- D[i]
      }
      else{
        sumR = 0
        for (j in x_s){
          sumR = sumR + MI(X[[i]],X[[j]])
        }
        R[i] = (1/length(x_s))*sumR
        
        P[i] <- D[i]-R[i]
      }
    }
    k <- which.max(P)
    x_s <- append(x_s,k)
    x_new <- setdiff(x_new,k)
    print(x_s)
  }
  return(x_s)
}

fset(mutinfo_mix,plantDAE8[,2:218],plantDAE8$Silique,7)
