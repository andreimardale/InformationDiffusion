#### Clustering Users in Topic Space and Determining the content of the clusters ###
source('./library_loader.R')
source("./utils.R")

t = initializeData()
PERIOD = 2

T_ = t[[PERIOD]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

tm_ = unique(as.matrix(getTermMatrixWithTM(t, PERIOD, sparsity = 0.98498730956292, weightTf)))
tm_ = tm_[rowSums(tm_) > 0,]

K = 10
model = LDA(tm_, k = K, method = "Gibbs", control = list(burnin = 500, thin = 100, iter = 4000, alpha = 0.1))
printTermsPerTopic(model, 10, PERIOD)

phi = topicmodels::posterior(model)$terms
theta = topicmodels::posterior(model)$topics
gamma = t(CalcGamma(phi, theta, p_docs = Matrix::rowSums(tm_)))
no_of_users = dim(theta)[1]
no_of_terms = dim(gamma)[1]

KLM_Users <- KL(theta)
tsne_Users = Rtsne(KLM_Users, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

minPts = 11
dbscan.result = dbscan::hdbscan(x = KLM_Users, minPts = minPts)
no_of_clusters = length(unique(dbscan.result$cluster))

png(paste0("Figures/dbscan_K", K, "_minpts", minPts, "_all.png"), width = 1800, height = 1800, res = 300)
plot(tsne_Users$Y[,1], tsne_Users$Y[,2],main=paste("hdbscan", "minpts", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2",
     col = adjustcolor(colPal[dbscan.result$cluster+1], alpha=0.5), pch=16)
legend("bottomleft", paste0("Cluster ", sort(unique(dbscan.result$cluster))),
       cex=.8, col=colPal[1 + sort(unique(dbscan.result$cluster))], pch = rep(16,8))
dev.off()


### Computing the centroids of the clusters ####
mean_clusters = matrix(ncol = K)[-1,]
sdev_clusters = c()

for (cluster in sort(unique(dbscan.result$cluster))) {
  theta_cluster = (theta[dbscan.result$cluster == cluster, ])
  mean_clusters = rbind(mean_clusters, apply(theta_cluster, 2, mean))
  sdev_clusters = c(sdev_clusters, mean(apply(theta_cluster, 2, sd)))
}

KLM_Users_Terms_Centroids <- KL(rbind(theta, gamma, mean_clusters))
tsne_Users_Terms_Centroids = Rtsne(KLM_Users_Terms_Centroids, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

### Plot Users, Terms and Centroids ####
png(paste0("Figures/tsne_users_centroids_lda_K", K, "_minpts", minPts, ".png"), width = 1800, height = 1800, res = 300)
plot(tsne_Users_Terms_Centroids$Y[1:no_of_users,1], tsne_Users_Terms_Centroids$Y[1:no_of_users,2],xlim = c(-33, 23), main=paste("hdbscan", "minpts", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colPal[dbscan.result$cluster+1], alpha=0.5), pch=16)
for (i in 2:no_of_clusters) {
  points(tsne_Users_Terms_Centroids$Y[(no_of_users + no_of_terms)+i,1], tsne_Users_Terms_Centroids$Y[(no_of_users + no_of_terms)+i,2], col = '#CAB2D6', pch = 15)
}
legend("bottomleft", c(paste0("Cl. ", sort(unique(dbscan.result$cluster))), "Center"), cex=.8, col=c(colPal[1 + sort(unique(dbscan.result$cluster))], "#CAB2D6"), pch = c(rep(16,no_of_clusters), 15) )
dev.off()
########
png(paste0("Figures/tsne_users_terms_centroids_lda_K", K, "_minpts", minPts, ".png"), width = 1800, height = 1800, res = 300)
plot(tsne_Users_Terms_Centroids$Y[1:no_of_users,1], tsne_Users_Terms_Centroids$Y[1:no_of_users,2],xlim = c(-33, 23), main=paste("hdbscan", "minpts", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colPal[dbscan.result$cluster+1], alpha=0.5), pch=16)
points(tsne_Users_Terms_Centroids$Y[(no_of_users+1):(no_of_users + no_of_terms), 1], tsne_Users_Terms_Centroids$Y[(no_of_users+1): (no_of_users + no_of_terms),2], col = adjustcolor('red', alpha=0.2), pch=16)
for (i in 2:no_of_clusters) {
  points(tsne_Users_Terms_Centroids$Y[(no_of_users + no_of_terms)+i,1], tsne_Users_Terms_Centroids$Y[(no_of_users + no_of_terms)+i,2], col = '#CAB2D6', pch = 15)
}
legend("bottomleft", c(paste0("Cl. ", sort(unique(dbscan.result$cluster))), "Center"), cex=.8, col=c(colPal[1 + sort(unique(dbscan.result$cluster))], "#CAB2D6"), pch = c(rep(16,no_of_clusters), 15) )
dev.off()

#### Computing closest terms to the Centroids ####
for (i in 2:no_of_clusters) {
  KLM_Centroid_Terms <- KL(rbind(mean_clusters[i,], gamma))
  rn_gamma = rownames(gamma)[order(KLM_Centroid_Terms[1,]) - 1][1:20]
  png(paste0("Figures/T", PERIOD, "_cluster_", (i-1), "_closest_words.png"), width = 3600, height = 1800, res = 170)
  grid.table(gamma[rn_gamma,])
  dev.off()
}
#####




no_of_posts = dim(t[[PERIOD]])[1]
post_probabilities = matrix(0, ncol = K)[-1,]

model_list <- TmParallelApply(X = 1:no_of_posts, FUN = function(p){
  tm_p = as.matrix(getTermMatrixWithTMForOneReply(t[[PERIOD]]$Content[p], sparsity = 0.98498730956292, weightTf))
  if(length(tm_p) == 0)
    return()
  
  post_probability = matrix(0, nrow = 1, ncol = K)
  for (i in 1:100) {
    post_probability <- post_probability +  topicmodels::posterior(model, tm_p)$topics
  }
  post_probabilities = rbind(post_probabilities, 0.01 * post_probability)
}, cpus = 20)

# save(model_list, file="post_probabilities.RData")


non_null_model_list = model_list
names(non_null_model_list) <- seq_along(non_null_model_list)
non_null_model_list = plyr::compact(non_null_model_list)


View(names(non_null_model_list))

post_probabilities <- data.frame(matrix(unlist(model_list, use.names = T), nrow=length(model_list), byrow=T))

test = data.frame(non_null_model_list)

KLM_Centroid_Posts = KL(rbind(mean_clusters[2,], post_probabilities))
order(KLM_Centroid_Posts[1,])




tm_p = as.matrix(getTermMatrixWithTMForOneReply(t[[PERIOD]]$Content[4], sparsity = 0.98498730956292, weightTf))
post_probability = matrix(0, nrow = 1, ncol = K)
topicmodels::posterior(model, tm_p)$topics




KLM_Centroid_Users <- KL(rbind(mean_clusters[2,], theta))
rn_theta = rownames(theta)[order(KLM_Centroid_Users[1,]) - 1][1:20]
rn_theta







### Uncomment to plot interactively ####
# tsne = Rtsne(KL(rbind(theta, gamma)), dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)
# hover_text <- character(0)
# for (i in 1 : dim(theta)[1]) {
#   hover_text[i] = substr(as.character(T_[i, "text"]), 1, 200)
# }
# hover_text = c(hover_text, colnames(tm_))
# 
# plotdata <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2], hover_text = hover_text)
# plt2 <- ggplot(plotdata) + geom_point(aes(x = tsne_x, y = tsne_y, text = hover_text), color = adjustcolor(c(rep(1, no_of_users), rep(2, no_of_terms)), alpha=0.5))
# ggplotly(plt2)
###

