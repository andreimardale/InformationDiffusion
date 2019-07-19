####  Clustering in Words Space ####

source('./2019.04.26_TSNE_users_in_words_space.R')

performKMedoids(cosine.dist, 2, tsne)
performKMedoids(cosine.dist, 3, tsne)
performKMedoids(cosine.dist, 4, tsne)
performKMedoids(cosine.dist, 5, tsne)
performKMedoids(cosine.dist, 6, tsne)
performKMedoids(cosine.dist, 7, tsne)
performKMedoids(cosine.dist, 8, tsne)
performKMedoids(cosine.dist, 9, tsne)

performHDBSCAN(cosine.dist, 2, tsne)
performHDBSCAN(cosine.dist, 4, tsne)
performHDBSCAN(cosine.dist, 5, tsne)
performHDBSCAN(cosine.dist, 6, tsne)
performHDBSCAN(cosine.dist, 7, tsne)
performHDBSCAN(cosine.dist, 8, tsne)
performHDBSCAN(cosine.dist, 9, tsne)


#### Thourugh analysis ####
K = 6
kmed = pam(cosine.dist, k = K, diss = TRUE, keep.diss = TRUE)

png("kmedoids_6_details.png", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main=paste("kmedoids", "k", K, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(kmed$clustering, alpha=0.5), pch=16)
dev.off()

for (cluster in 1:K) {
  png(paste("kmedoids", "cluster", cluster, sep = "_"), width = 1600, height = 1600, res = 300)
  plot(tsne$Y[kmed$clustering == cluster,1], tsne$Y[kmed$clustering == cluster,2],main=paste("kmedoids", "cluster", cluster, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(cluster, alpha=0.5), pch=16)
  dev.off()
  
  t_cluster = sort(colSums(tm_13_uniq[kmed$clustering== cluster, ]), decreasing = TRUE)
  d_cluster = data.frame(word = names(t_cluster),freq=t_cluster)
  
  plotWordCloud(d_cluster, paste("wordcloud", "cluster", cluster, sep = "_"))
}

