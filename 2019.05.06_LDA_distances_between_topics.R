#### Distance analysis in topic space ###
source('./library_loader.R')
source("./utils.R")

# Initialize data
t = initializeData()
PERIOD = 2

tm_ = unique(as.matrix(getTermMatrixWithTM(t, PERIOD, sparsity = 0.98498730956292, weightTf)))
tm_ = tm_[rowSums(tm_) > 0,]

K = 10
model = LDA(tm_, k = K, method = "Gibbs", control = list(burnin = 1000, iter = 2000, keep = 50, alpha = 0.1))

png(paste0(paste0("lda", K),"_topics_T", PERIOD, ".png"), width = 3200, height = 1800, res = 300)
termsPerTopic(model) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  ggtitle("Top 10 words for each discovered topic")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
dev.off()


terms_T1 = topicmodels::posterior(model)$terms
KLMatrix_Topics = KL(terms)


png(paste0(paste0("lda", K),"_heatmap_T", PERIOD, ".png"), width = 1600, height = 1600, res = 300)
par(mfrow=c(2,1))
heatmap.2(KLMatrix_Topics, main = "KL-Dist between topics ", labRow = as.character(1:10), labCol = as.character(1:10), dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()


terms_10_30 = smartbind(terms10, terms30, fill = 0)
terms_10_after = terms_10_30[1:10, ]
terms_30_after = terms_10_30[11:20,]

distP2P4 = 0
for (i in 1:K) {
  for (j in 1:K) {
    distP2P4 = distP2P4 + kullback_leibler_distance(as.matrix(terms_10_after[i, ], nrow = 1), as.matrix(terms_30_after[j, ], nrow = 1),
                                                    testNA = T, unit = "log2")
  }
}



