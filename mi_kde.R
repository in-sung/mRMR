KDE_MI <- function(x,y)
{
  n <- length(x)
  sigma_x <- sd(x)
  h_x <- 1.06*sigma_x*n^(-1/5)
  density_x <- double(n)
  for(i in 1:n){
    sumG = 0
    for(j in 1:n){
      sumG <- sumG + exp(-(x[i]-x[j])*(x[i]-x[j])/(2*h_x*h_x))
    }
    density_x[i] <- sumG/(n*h_x*sqrt(2*pi))
  }
  sigma_y <- sd(y)
  h_y <- 1.06*sigma_y*n^(-1/5)
  density_y <- double(n)
  for(i in 1:n){
    sumG = 0
    for(j in 1:n){
      sumG <- sumG + exp(-(y[i]-y[j])*(y[i]-y[j])/(2*h_y*h_y))
    }
    density_y[i] <- sumG/(n*h_y*sqrt(2*pi))
  }
  sigma_xy <- (sigma_x+sigma_y)/2
  h_xy <- sigma_xy*n^(-1/6)
  density_xy <- double(n)
  for(i in 1:n){
    sumE =0
    for(j in 1:n){
      sumE <- sumE + exp(-((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]))/(2*h_xy*h_xy))
    }
    density_xy[i] <- sumE/(n*h_xy*h_xy*2*pi)
  }
  mi <- mean(log(density_xy/density_x*density_y))
  return(mi)
}
