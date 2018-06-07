#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Group 9
### ADS Spring 2018


train <- function(dat_train, label_train,
                  run.xgboost = F, run.gbm = F,
                  run.adaboost = F,
                  par=NULL,
                  num_class = 3){
  
  ### train a model according to input
  
  ### Input: 
  ###   dat_train -  processed features from images 
  ###   label_train -  class labels for training images
  ###   run.xxxxxx - select which model to fit
  ###   par - specified parameters, otherwise use default
  ### Output: 
  ###   fitted model
  
  ### load libraries
  library("gbm")
  library("adabag")
  library("xgboost")
  
  
  ### fit selected model
  
  
  #### gradient boosting model
  
  if(run.gbm == T){
    
    if(is.null(par)){
      ntrees = 100
      shrinkage = 0.1
    }
    else{
      ntrees = par$ntrees
      shrinkage = par$shrinkage
    }

    fit.model <- gbm.fit(x = dat_train,
                         y = factor(label_train),
                         interaction.depth = 3, 
                         shrinkage = shrinkage,
                         bag.fraction = 0.5,
                         n.trees = ntrees,
                         verbose = FALSE,
                         distribution="multinomial")
  }
  
  ###############################################################
  
  ####  adaboosting model
  
  if(run.adaboost == T){
    
    # load parameter
    if(is.null(par)){
      mfinal <- 100
    } else {
      mfinal <- par$mfinal
    }
    
    # convert trainning data to data frame
    train <- data.frame(label = factor(label_train), data = dat_train)
    
    # fit model
    fit.model <- boosting(label~.,data = train,
                          mfinal = mfinal, 
                          coeflearn= "Zhu")
  }
  
  
  ###############################################################
  
  #### xgboost model
  
  if(run.xgboost == T){
    if(is.null(par)){
      depth <- 5
      child_weight <- 3
    } else {
      depth <- par$depth
      child_weight <- par$child_weight
    }
    
    # create xgb.Dmatrix
    train_matrix <- xgb.DMatrix(data=data.matrix(dat_train),label=label_train)

    
    # fit xgboost model
    fit.model <- xgb.train(data = train_matrix,
                          max.depth = depth,
                          min_child_weight = child_weight,
                          eta = 0.3,
                          nthread = 4,
                          nround = 100,
                          num_class = num_class)
  }
  return(fit.model)
}
