#### Distance analysis in topic space ###
source('./library_loader.R')
source("./utils.R")

# Initialize data
t = initializeData()

## Set verbose to TRUE to print distances from every topic to every topic in each period ##
verbose = FALSE
number_of_cpus = 3
# number_of_cpus = 13
##

K = 10  ## The number of topics ##
period_list <- c(1, 2, 3, 4)
# period_list <- c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15)
P = length(period_list)

model_list <- TmParallelApply(X = period_list, FUN = function(p){
  tm_ = unique(as.matrix(getTermMatrixWithTM(t, p, sparsity = 0.98498730956292, weightTf)))
  tm_ = tm_[rowSums(tm_) > 0,]
  
  model = LDA(tm_, k = K, method = "Gibbs", control = list(burnin = 1000, iter = 2000, keep = 50, alpha = 0.1))
  
  png(paste0(paste0("lda", K),"_topics_T", p, ".png"), width = 3200, height = 1800, res = 300)
  r = termsPerTopic(model) %>%
    mutate(term = reorder(term, beta))
  
  print(ggplot(data = r, aes(term, beta, fill = factor(topic))) +
          ggtitle("Top 10 words for each discovered topic")+
          geom_col(show.legend = FALSE) +
          facet_wrap(~ topic, scales = "free") +
          coord_flip())
  dev.off()
  
  
  KLMatrix_Topics = KL(topicmodels::posterior(model)$terms)
  KLMatrix_Topics[lower.tri(KLMatrix_Topics, diag = TRUE)]<- NA
  png(paste0(paste0("lda", K),"_heatmap_T", p, ".png"), width = 1600, height = 1600, res = 300)
  heatmap.2(KLMatrix_Topics, main = paste0("KL-Dist of topics in T", p), labRow = as.character(1:K), labCol = as.character(1:K), dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
  dev.off()
  
  return(model)
  
}, cpus = number_of_cpus)


distances <- matrix(0, nrow = P, ncol = P)
colnames(distances) <- as.character(period_list)
for (p1 in 1:P) {
  for (p2 in (p1) : P) {
    if(verbose == TRUE) {
      print(paste("Computing distance from period ", p1, "to", p2))
    }

    terms1 = topicmodels::posterior(model_list[[p1]])$terms
    terms2 = topicmodels::posterior(model_list[[p2]])$terms
    
    terms_12 = smartbind(terms1, terms2, fill = 0)
    terms_1_after = terms_12[1:K, ]
    terms_2_after = terms_12[(K+1) : dim(terms_12)[1],]
    
    distTotal = 0
    for (i in 1:K) {
      dist12 = Inf
      minj = Inf
      for (j in 1:K) {
        d1 = (kullback_leibler_distance(as.matrix(terms_1_after[i, ], nrow = 1), as.matrix(terms_2_after[j, ], nrow = 1),
                                        testNA = T, unit = "log2"))
        
        d2 = (kullback_leibler_distance(as.matrix(terms_2_after[j, ], nrow = 1), as.matrix(terms_1_after[i, ], nrow = 1),
                                        testNA = T, unit = "log2"))
        
        d = (d1 + d2) / 2
        if (d < dist12) {
          dist12 = d
          minj = j
        }
      }
      if(verbose == TRUE){
        print(paste("Distance", i, minj, "is", dist12 ))
      }
      
      distTotal = distTotal + dist12
    }
    
    distances[p1,p2] = distTotal
    if(verbose == TRUE){
      print(distTotal)
    }
    
  }
}

distances[lower.tri(distances, diag = TRUE)]<- NA
png(paste0(paste0("lda", K),"_distance_between_periods", ".png"), width = 1600, height = 1600, res = 300)
heatmap.2(distances, main = "KL-Dist between periods ", labRow = as.character(1:P), labCol = as.character(1:P), dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()
