#### Clustering Users in Topic Space ###
source('./library_loader.R')
source("./utils.R")

# Initialize data
t = initializeData()
PERIOD = 3

T_ = t[[PERIOD]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

tm_ = unique(as.matrix(getTermMatrixWithTM(t, PERIOD, sparsity = 0.98498730956292, weightTf)))
tm_ = tm_[rowSums(tm_) > 0,]

K = 10
model = LDA(tm_, k = K, method = "Gibbs", control = list(burnin = 1000, iter = 2000, keep = 50, alpha = 0.1))

printTermsPerTopic(model, 10, PERIOD)

theta = topicmodels::posterior(model)$topics
KLMatrix <- KL(theta)

#### Clustering in Topic space ####
tsne = Rtsne(KLMatrix, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

png(paste0("T", PERIOD, "_K", K, "_usersInTopicSpace.png"), width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2], xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()

for (minPts in (7:10)) {
  performHDBSCAN(KLMatrix, minPts, tsne)
}

minPts = 10
dbscan.result = dbscan::hdbscan(x = KLMatrix, minPts = minPts)

png(paste0("dbscan_K", K, "_minpts", minPts, "_all.png"), width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main=paste("hdbscan", "minpts", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2",
     col = adjustcolor(colPal[dbscan.result$cluster+1], alpha=0.5), pch=16, xlim = c(-15, 15), ylim = c(-15, 15))
legend("bottomleft", paste0("Cluster ", sort(unique(dbscan.result$cluster))),
       cex=.8, col=colPal[1 + sort(unique(dbscan.result$cluster))], pch = rep(16,8))
dev.off()

#######

for (i in 1:length(sdev_clusters)) {
  print(paste("Cluster ", i, "|sdev:", sdev_clusters[i]))
}

for (cluster in unique(dbscan.result$cluster)) {
  png(paste("hdbscan", "cluster", cluster, sep = "_"), width = 1600, height = 1600, res = 300)
  plot(tsne$Y[dbscan.result$cluster == cluster,1], tsne$Y[dbscan.result$cluster == cluster,2],
       main=paste("hdbscan", "cluster", cluster, sep = "_"), xlab="Dim1", ylab = "Dim2",
       col = adjustcolor(colPal[cluster + 1L], alpha=0.5), pch=16, xlim = c(-20, 20), ylim = c(-20, 20))
  dev.off()
  
  t_cluster = sort(colSums(tm_[dbscan.result$cluster == cluster, ]), decreasing = TRUE)
  d_cluster = data.frame(word = names(t_cluster),freq=t_cluster)
  
  plotWordCloud(d_cluster, paste("wordcloud", "cluster", cluster, sep = "_"))
}
