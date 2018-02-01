########### generate data ############
x1 <-rnorm(400,0,10)
x2 <-rgeom(400,1/35)
x3 <-rbinom(400,65,1/15)
x4 <-rt(400,14)
x5 <-rnorm(400,15,25)
x6 <-rt(400,30)
x7 <-rpois(400,6)
x8 <-rnbinom(400,10,1/16)
x9 <-runif(400,0,10)
y <-(x1*4)+(x3^2)+sin(x5)+(x6*10)+x7
data_demo <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,y)

############ feature selection ############
fset(KDE_MI,data_demo[,1:9],data_demo[,10],5) #(1,8,2,5,9)
fset(mutinfo.R,data_demo[,1:9],data_demo[,10],5) #(1,3,9,6,4)
fset(mutinfo_mix,data_demo[,1:9],data_demo[,10],5) #(1,8,3,2,7)

############# predict-SVM ###############
library(e1071)
mse1 <- matrix(nrow=1000,ncol=1)

for(i in 1:1000){
  RandID <- sample(nrow(data_demo))
  PermutedData <- data_demo[RandID,]
  train_pl <- PermutedData[1:300,]
  test_pl <- PermutedData[301:400,]
  
  #model_kde <- svm(y~ x1+ x8+ x2 +x5+ x9 ,data=train_pl,type="nu-regression",nu=0.01,
   #                  kernel="radial",cost=16,gamma=0.001 )
  
  model_mi_r <- svm(y~ x1+ x3 +x9 +x6 +x4 ,data=train_pl,type="nu-regression",nu=0.01,
                    kernel="radial",cost=32,gamma=0.01)
  
  #model_mi_mix <- svm(y~ x1+ x8 +x3 +x2 +x7 , data=train_pl,type="nu-regression",nu=0.01,
   #            kernel="radial",cost=32,gamma=0.01)
  
  #svm_pred1 <- predict(model_kde,test_pl)
  svm_pred1 <- predict(model_mi_r,test_pl)
  #svm_pred1 <- predict(model_mi_mix,test_pl)
  error1 <- svm_pred1 - test_pl$y
  
  mse1[i] <-sum((error1)^2)/length(error1)
  if(i%%10 == 0 ){
    print(i)
  }
}
sum(mse1)/1000

################## predict-linear regression ######################
mse1 <- matrix(nrow=1000,ncol=1)

for(i in 1:1000){
  RandID <- sample(nrow(data_demo))
  PermutedData <- data_demo[RandID,]
  train_pl <- PermutedData[1:300,]
  test_pl <- PermutedData[301:400,]
  
  model_kde <- lm(y~ x1+ x8+ x2 +x5+ x9 ,data=train_pl)
  
  #model_mi_r <- lm(y~ x1+ x3 +x9 +x6 +x4 ,data=train_pl)
  
  #model_mi_mix <- lm(y~ x1+ x8 +x3 +x2 +x7 , data=train_pl)
  
  lm_pred <- predict(model_kde,test_pl)
  #lm_pred <- predict(model_mi_r,test_pl)
  #lm_pred <- predict(model_mi_mix,test_pl)
  error <- lm_pred - test_pl$y
  
  mse1[i] <-sum((error)^2)/length(error)
  if(i%%10 == 0 ){
    print(i)
  }
}
sum(mse1)/1000
