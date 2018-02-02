# stepwise regression
# alpha1 = 0.1, alpha2 = 0.15 by default
# y: response
# X: predictor matrix


my_stepwise <- function(y,X,alpha1 = 0.1, alpha2 = 0.15){
  
  flag <- 0;
  subset <- c()
  allset <- 1:dim(X)[2];
  while(flag == 0){
    if(length(subset) >  0){
      subremain <- allset[-subset];
    }else{
      subremain <- allset
    }
    Xcurrent <- X[,subset];
    isuse <- 0;
    
    # add predictor
    for(i in subremain){
      Xtry <- cbind(X[,subset],X[,i])
      fit <- lm(y ~ Xtry);
      pos <- length(subset) + 2;
      pvalue <- coef(summary(fit))[pos,4]
      if(pvalue < alpha1){
        subset <- c(subset,i);
        isuse <- 1;
        break;
      }
    }
    
    print("current predictor: ")
    print(subset)
    
    # delete predictor
    Xnew <- cbind(Xcurrent, X[,subset[length(subset)]]);
    fit <- lm(y~Xnew);
    pvec <- coef(summary(fit))[-1,4]
    indmax <- which.max(pvec);
    if(pvec[indmax] > alpha2){
      subset <- subset[-indmax];
      print("delete predictor: ")
      print(subset[indmax])
    }
    
    if(isuse == 0 && pvec[indmax] < alpha2 ){
      flag <- 1;
    }
    
    
  }
  
  print("final predictor: ")
  print(subset)
  
}

