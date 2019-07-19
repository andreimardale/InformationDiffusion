library(reticulate)
use_python("/usr/bin/python3", required=TRUE)

require(parallel)
require(caret)
library(ggplot2)
require(dplyr)

scripts <- c('F0_075_load.py', 'F1_075_load.py', 'F2_075_load.py', "F3_075_load.py", "F123_075_extra_load.py", 'F0123_075_load.py')


.cl <- makeCluster(spec = min(detectCores(), length(scripts)))
res <- parSapply(cl = .cl, X = scripts, FUN = function(myscript) {
  library(reticulate)
  use_python("/usr/bin/python3", required=TRUE)
  
  require(caret)
  
  ## now, we execute the python script which simply loads the dill with everything saved in it!
  ## note that it might take a while to load the dill file
  source_python(myscript)
  
  ## now, simple process it!
  models <- c("logistic_regression", "KNN", "random_forest", "gradient_boosting", "xgboost")
  
  algo_names <- c("KNN", "Logistic\nRegression", "Random\nForests", "Gradient\nBoosting", "XGboost")
  
  myLabels <- c("True", "Predicted")
  results <- list()
  for (model in models) {
    
    ## for each fold
    results[[model]] <- data.frame(t(sapply(X = 1:length(predicted_labels[[model]]), FUN = function(foldno) {
      pred <- data.frame(True = unlist(true_labels[[model]][[foldno]]), Predicted = unlist(predicted_labels[[model]][[foldno]]))
      names(pred) <- myLabels
      
      ## transform into factors
      pred$True <- as.factor(pred$True)
      pred$Predicted <- factor(x = pred$Predicted, levels = levels(pred$True))
      
      ## compute measures
      result <- confusionMatrix(data = pred$Predicted, reference = pred$True, mode="prec_recall")
      result$byClass[is.na(result$byClass)] <- 0
      apply(X = result$byClass, MARGIN = 2, FUN = mean, na.rm = T) #
    })))
  }
  return(list(results))
})
stopCluster(cl = .cl)
names(res) <- c("FS0", "FS1", "FS2", "FS3", "FS4", "FS5")

save(res, file = "F_all_performances.dat", compress = "xz")
