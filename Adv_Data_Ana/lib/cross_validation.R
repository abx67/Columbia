########################
### Cross Validation ###
########################

### Author: Group 9
### ADS Spring 2018

###cross validation function 

#Cross Validatio
cv <- function(dat_train ,label_train,
               run.xgboost = F,
               run.gbm = F,
               run.adaboost = F,
               K = 5, 
               par = NULL,
               num_class = 3){
  
  ### load libraries
  library("gbm")
  library("adabag")
  library("xgboost")
  
  ### load funcions 
  source("./lib/train.R")
  source("./lib/test.R")
  
  
  n <- length(label_train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    print(paste0("current iteration is ", i))
    train.data <- dat_train[s != i,]
    train.label <- label_train[s != i]
    test.data <- dat_train[s == i,]
    test.label <- label_train[s == i]
    
    ## choose model
    
    # run adaboost model
    if(run.adaboost == T){
      fit.model <- train(train.data, train.label, run.adaboost = T, par = par)
      pred <- test(fit.model, test.data, run.adaboost = T, par = par)
    }
    
    # run gbm model
    if(run.gbm == T){
      fit.model <- train(train.data, train.label, run.gbm = T, par = par)
      pred <- test(fit.model, test.data, run.gbm = T, par=par)
    }
    
    # run xgboost model
    if(run.xgboost == T){
      fit.model <- train(train.data, train.label, run.xgboost = T, par = par, num_class = num_class)
      pred <- test(fit.model, test.data, run.xgboost = T, par = par)
    }
    
    ## calculate cv error
    cv.error[i] <- mean(pred != test.label)
  }		
  
  return(list(error = mean(cv.error), sd = sd(cv.error)) )
}
