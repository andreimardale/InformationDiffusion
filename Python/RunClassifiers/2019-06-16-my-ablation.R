library(reticulate)
use_python("/usr/bin/python3", required=TRUE)

require(parallel)
require(caret)
library(ggplot2)
require(dplyr)

source_python("F0_075_load.py")

model = "xgboost"
  
myLabels <- c("Prev", "True", "Predicted")

results <- list()

## for each fold
results[[model]] <- data.frame(t(sapply(X = 1:length(predicted_labels[[model]]), FUN = function(foldno) {
  previous_label = do.call("rbind", lapply(datasets[[foldno]][[3]], "[[", length(datasets[[1]][[1]][[1]]))) ### Change 16 with the number of features
  
  pred <- data.frame(Previous = previous_label, True = unlist(true_labels[[model]][[foldno]]), Predicted = unlist(predicted_labels[[model]][[foldno]]))
  names(pred) <- myLabels
  
  ## transform into factors
  pred$Prev <- as.factor(pred$Prev)
  pred$True <- as.factor(pred$True)
  pred$Predicted <- factor(x = pred$Predicted, levels = levels(pred$True))
  
  # pred_ = pred %>% group_by(Prev, True) %>% summarise(count = n())
  
  return(pred)
})))
  
Prev <- unlist(results[[model]][["Prev"]], recursive=FALSE)

True <- unlist(results[[model]][["True"]], recursive=FALSE)

Predicted <- unlist(results[[model]][["Predicted"]], recursive=FALSE)

all_results = data.frame(Prev, True, Predicted)

# result <- confusionMatrix(data = all_results$Predicted, reference = all_results$True, mode="prec_recall")
# apply(X = result$byClass, MARGIN = 2, FUN = mean, na.rm = T)

all_results_ = all_results %>% group_by(Prev, True) %>% summarise(count = n())

#####

#### Chaange  ####
result_per_class = list()
k = 1
for (i in 0:2) {
  for(j in 0:2) {
    c1 = all_results[all_results$Prev == i & all_results$True == j,]
    result <- confusionMatrix(data = c1$Predicted, reference = c1$True, mode="prec_recall")
    r1 = apply(X = result$byClass, MARGIN = 2, FUN = mean, na.rm = T)
    
    result_per_class[[k]] = r1
    k = k + 1
  }        
}

res = c()
for(i in 1 : 9) {
  if(!is.na(result_per_class[[i]]["F1"]))
    res = c(res, result_per_class[[i]]["F1"])
  else
    res = c(res, result_per_class[[i]]["F1"])

}

mat = matrix(res, nrow = 3, ncol = 3, byrow = T)
mat = data.frame(mat)
colnames(mat) = c("0", "1", "2")
rownames(mat) = c("0", "1", "2")

View(mat)

# toPlot = data.frame(c("Against", "Brexit", "Neutral"), t(mat))
# colnames(toPlot) = c("Predicted Stance", "F1")
# 
# ggplot(toPlot, aes(x = toPlot$`Predicted Stance`, y = toPlot$F1, fill = toPlot$`Predicted Stance`)) + 
#   geom_bar(stat="identity", position="dodge") +  
#   scale_x_discrete(name = element_blank(), labels=c("Against", "Brexit", "Neutral")) + #name = "Classifier algorithms"
#   scale_y_continuous(name = "Value of measure", breaks = seq(0, 1, 0.1), limits=c(0, 1)) + #limits=c(0.475, 0.565)
#   ggtitle("Prediction performance: F1") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22, face = "bold"), #family = "Tahoma", -- not supported by PDF
#         text = element_text(size = 18), #family = "Tahoma", -- not supported by PDF
#         axis.title = element_text(face="bold"),
#         axis.text.x = element_text(size = 16),
#         legend.position = "bottom") +
#   scale_fill_brewer(palette = "Paired") +
#   labs(fill = "Features:")






################# Statistical Testing ########################
model = "xgboost"

myLabels <- c("Predicted", "True")
featureNames = c("FS0", "FS2")
columnNames = c("True", "False")

data_table = data.frame()
k = 1

for (features in c('F0_075_load.py', "F2_075_load.py")) {
  source_python(features)
  results <- list()
  
  ## for each fold
  results[[model]] <- data.frame(t(sapply(X = 1:length(predicted_labels[[model]]), FUN = function(foldno) {
    
    pred <- data.frame(True = unlist(true_labels[[model]][[foldno]]), Predicted = unlist(predicted_labels[[model]][[foldno]]))
    names(pred) <- myLabels
    
    ## transform into factors
    pred$True <- as.factor(pred$True)
    pred$Predicted <- factor(x = pred$Predicted, levels = levels(pred$True))
    
    return(pred)
  })))
  
  True <- unlist(results[[model]][["True"]], recursive=FALSE)
  Predicted <- unlist(results[[model]][["Predicted"]], recursive=FALSE)
  
  all_results = data.frame(Predicted, True)
  
  data_table[k,1] = sum(all_results$True == all_results$Predicted)
  data_table[k,2] = sum(all_results$True != all_results$Predicted)
  k = k + 1
}

colnames(data_table) = columnNames
rownames(data_table) = featureNames

print(chisq.test(data_table))




