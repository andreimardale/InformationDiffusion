#### Clustering Users in Topic Space ###
source('./library_loader.R')
source("./utils.R")

nb_big_nohash = readRDS("../Data/correct_model_no_hashtags.rds")


# Initialize data 
t = initializeData()
PERIOD = 12

t[[PERIOD]]$Content = str_replace_all(str_replace_all(t[[PERIOD]]$Content, "&gt;.*\n", ""), "\n", "")

T_ = t[[PERIOD]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

T_rez1 = getAggregatePredictResults(t[[PERIOD]])
T_sorted = T_rez1[order(T_rez1$leave_probability, decreasing = T),]

View(T_sorted[31:40,])


T_ = t[[p]]
colnames(T_)[3] = "text"

tm_ = unique(as.matrix(getTermMatrixWithTM(t, PERIOD, sparsity = 0.98498730956292, weightTf)))
tm_ = tm_[rowSums(tm_) > 0,]

K = 10
model = LDA(tm_, k = K, method = "Gibbs", control = list(burnin = 500, thin = 100, iter = 4000, alpha = 0.1))

printTermsPerTopic(model, 10, PERIOD)

theta = topicmodels::posterior(model)$topics
KLMatrix <- KL(theta)

#### Clustering in Topic space ####
tsne = Rtsne(KLMatrix, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

# png(paste0("T", PERIOD, "_K", K, "_usersInTopicSpace.png"), width = 1600, height = 1600, res = 300)
# plot(tsne$Y[,1], tsne$Y[,2], xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
# dev.off()

# for (minPts in (10:20)) {
#   performHDBSCAN(KLMatrix, minPts, tsne)
# }


minPts = 10
dbscan.result = performKMedoids(KLMatrix, minPts, tsne)

png(paste0("Kmedoids_topics_", K, "_K", minPts, "_centroids.png"), width = 2000, height = 2000, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main=paste("kmedoids", "k", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2",
     col = adjustcolor(specPal_T14[dbscan.result$cluster], alpha=0.4), pch=16)
dev.off()

#######


printTermsPerTopic(model, 10, PERIOD)

#####


for (cluster in unique(dbscan.result$cluster)) {
  # png(paste("hdbscan", "cluster", cluster, sep = "_"), width = 1600, height = 1600, res = 300)
  # plot(tsne$Y[dbscan.result$cluster == cluster,1], tsne$Y[dbscan.result$cluster == cluster,2],
  #      main=paste("hdbscan", "cluster", cluster, sep = "_"), xlab="Dim1", ylab = "Dim2",
  #      col = adjustcolor(colPal[cluster + 1L], alpha=0.5), pch=16, xlim = c(-20, 20), ylim = c(-20, 20))
  # dev.off()
  
  t_cluster = sort(colSums(tm_[dbscan.result$cluster == cluster, ]), decreasing = TRUE)
  d_cluster = data.frame(word = names(t_cluster),freq=t_cluster)
  
  plotWordCloud(d_cluster, paste("wordcloud", "cluster", cluster, sep = "_"))
}








######################################

tm_2 = unique(as.matrix(getTermMatrixWithTM(t, 2, sparsity = 0.98498730956292, weightTf)))
tm_2 = tm_2[rowSums(tm_2) > 0,]

tm_14 = unique(as.matrix(getTermMatrixWithTM(t, 14, sparsity = 0.98498730956292, weightTf)))
tm_14 = tm_14[rowSums(tm_14) > 0,]

common_authors = intersect(rownames(tm_2), rownames(tm_14))

K = 10

# model_2 = LDA(tm_2, k = K, method = "Gibbs", control = list(burnin = 500, thin = 100, iter = 4000, alpha = 0.1))
printTermsPerTopic(model_2, 10, 2)

theta_2 = topicmodels::posterior(model_2)$topics
KLMatrix_2 <- KL(theta_2)
tsne_2 = Rtsne(KLMatrix_2, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

kmeds.results_2 = performKMedoids(KLMatrix_2, 10, tsne_2, F)




##############################

# model_14 = LDA(tm_14, k = K, method = "Gibbs", control = list(burnin = 500, thin = 100, iter = 4000, alpha = 0.1))
printTermsPerTopic(model_14, 10, 14)

theta_14 = topicmodels::posterior(model_14)$topics
KLMatrix_14 <- KL(theta_14)
tsne_14 = Rtsne(KLMatrix_14, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

kmeds.results_14 = performKMedoids(KLMatrix_14, 10, tsne_14, F)

#####################################


ca1 = 40 # OK
ca2 = 13 # OK
ca3 = 33 # OK
ca4 = 28 # OK
ca5 = 45 # OK

position1_2 = which(as.data.frame(rownames(theta_2)) == common_authors[ca1], arr.ind=TRUE)[1]
position2_2 = which(as.data.frame(rownames(theta_2)) == common_authors[ca2], arr.ind=TRUE)[1]
position3_2 = which(as.data.frame(rownames(theta_2)) == common_authors[ca3], arr.ind=TRUE)[1]
position4_2 = which(as.data.frame(rownames(theta_2)) == common_authors[ca4], arr.ind=TRUE)[1]
position5_2 = which(as.data.frame(rownames(theta_2)) == common_authors[ca5], arr.ind=TRUE)[1]

png("T_2_10medoids_10topics.png", width = 2000, height = 2000, res = 300)
plot(tsne_2$Y[,1], tsne_2$Y[,2],main="T2", xlab="Dim1", ylab = "Dim2",
     col = adjustcolor(specPal_T2[kmeds.results_2$clustering], alpha=0.4), pch=16, ylim = c(-17, 25))
points(tsne_2$Y[position1_2,1], tsne_2$Y[position1_2,2], col = 1, lwd = 2, pch = 8)
points(tsne_2$Y[position2_2,1], tsne_2$Y[position2_2,2], col = 2, lwd = 2, pch = 8)
points(tsne_2$Y[position3_2,1], tsne_2$Y[position3_2,2], col = 4, lwd = 2, pch = 8)
points(tsne_2$Y[position4_2,1], tsne_2$Y[position4_2,2], col = colPal[10], lwd = 2, pch = 8)
points(tsne_2$Y[position5_2,1], tsne_2$Y[position5_2,2], col = colPal[2], lwd = 2, pch = 8)

legend("topright", c("Agreement", "Foreign Affairs", "Political Actors", "Mixed", "Politics", "Immigration", "Consequences",
                       "Debate", "Internal Affairs", "Economy"),
       cex=.8, col=specPal, pch = rep(16,8))

dev.off()




position1 = which(as.data.frame(rownames(theta_14)) == common_authors[ca1], arr.ind=TRUE)[1]
position2 = which(as.data.frame(rownames(theta_14)) == common_authors[ca2], arr.ind=TRUE)[1]
position3 = which(as.data.frame(rownames(theta_14)) == common_authors[ca3], arr.ind=TRUE)[1]
position4 = which(as.data.frame(rownames(theta_14)) == common_authors[ca4], arr.ind=TRUE)[1]
position5 = which(as.data.frame(rownames(theta_14)) == common_authors[ca5], arr.ind=TRUE)[1]

png("T_14_10medoids_10topics.png", width = 2000, height = 2000, res = 300)
plot(tsne_14$Y[,1], tsne_14$Y[,2],main="T14", xlab="Dim1", ylab = "Dim2",
     col = adjustcolor(specPal_T14[kmeds.results_14$clustering], alpha=0.4), pch=16)
points(tsne_14$Y[position1,1], tsne_14$Y[position1,2], col = 1, lwd = 2, pch = 8)
points(tsne_14$Y[position2,1], tsne_14$Y[position2,2], col = 2, lwd = 2, pch = 8)
points(tsne_14$Y[position3,1], tsne_14$Y[position3,2], col = 4, lwd = 2, pch = 8)
points(tsne_14$Y[position4,1], tsne_14$Y[position4,2], col = colPal[10], lwd = 2, pch = 8)
points(tsne_14$Y[position5,1], tsne_14$Y[position5,2], col = colPal[2], lwd = 2, pch = 8)

legend("bottomleft", c("Mixed","Extension", "Economy", "Dissatisfaction", "Remain Petition", "External Issues", "Ireland", "Second Vote",
                       "Politics", "May's Deal"),
       cex=.8, col=specPal, pch = rep(16,8))

dev.off()
