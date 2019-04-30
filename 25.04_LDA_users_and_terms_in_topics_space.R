source('./library_loader.R')
source("./utils.R")

#### Plotting Users and Terms in Topic Space ####
T_ = t[[13]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

tm_13 = as.matrix(getTermMatrixWithTM(t, 13, sparsity = 0.98498730956292, weightTf))
dim(tm_13)
tm_13 = unique(tm_13)
dim(tm_13)
tm_13 = tm_13[rowSums(tm_13) > 0,]


set.seed(12345)
K = 60
model <- FitLdaModel(dtm = tm_13, k = K, iterations = 2000, burnin = 180, alpha = 0.1, beta = 0.05,
                     optimize_alpha = TRUE, calc_likelihood = TRUE, calc_r2 = TRUE, cpus = 3)

png(paste0(paste("lda", K, "logliloglikelihood.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
plot(model$log_likelihood, type="l", main = paste("Log Likelihood - 50 topics - R2 score", model$r2, sep = " "))
dev.off()

#### Print topics ####
tidy_beta <- data.frame(topic = as.integer(stringr::str_replace_all(rownames(model$phi), "t_", "")), 
                        model$phi, 
                        stringsAsFactors = FALSE) %>%
  gather(term, beta, -topic) %>% 
  tibble::as_tibble()

ap_top_terms <- tidy_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


png(paste0(paste("lda", K, "topics.", sep = "_"), "png"), width = 3200, height = 1800, res = 300)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
dev.off()
#####
      
A = rbind(model$theta, t(model$gamma))
KLMatrix <- KL(A)
after.pca = cmdscale(KLMatrix, 50)

colors = c(rep(1, dim(model$theta)[1]), rep(2, dim(t(model$gamma))[1]))

tsne = Rtsne(KLMatrix, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)
png(paste0(paste("lda", K, "tsne_NoPCA.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
par(mfrow=c(1,2))
plot(tsne$Y[,1], tsne$Y[,2],main=paste0(K,"_LDA + KL + NoPCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
legend("topleft",c("Users","Terms"), cex=.8, col=c("black","red"),pch=c(16,16))
plot(tsne$Y[1: dim(model$theta)[1],1], tsne$Y[1: dim(model$theta)[1],2],main=paste0(K,"_LDA + KL + NoPCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
legend("topleft",c("Users"), cex=.8, col=c("black"),pch=c(16))
dev.off()

tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
png(paste0(paste("lda", K, "tsne_PCA.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
par(mfrow=c(1,2))
plot(tsne$Y[,1], tsne$Y[,2],main=paste0(K,"_LDA + KL + PCA + tSNE"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
legend("topleft",c("Users","Terms"), cex=.8, col=c("black","red"),pch=c(16,16))
plot(tsne$Y[1: dim(model$theta)[1],1], tsne$Y[1: dim(model$theta)[1],2],main=paste0(K,"_LDA + KL + PCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
legend("topleft",c("Users"), cex=.8, col=c("black"),pch=c(16))
dev.off()

### Uncomment to plot interactively ###
# hover_text <- character(0)
# for (i in 1 : dim(model$theta)[1]) {
#   hover_text[i] = substr(as.character(T_[i, "text"]), 1, 200)
# }
# hover_text = c(hover_text, colnames(tm_13))
# 
# plotdata <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2], hover_text = hover_text)
# plt2 <- ggplot(plotdata) + geom_point(aes(x = tsne_x, y = tsne_y, text = hover_text), color = adjustcolor(colors, alpha=0.5))
# ggplotly(plt2)
###



#### [TO_MOVE] Clustering in Topic space ####
KLMatrix <- KL(model$theta)

tsne = Rtsne(KLMatrix, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)
png("users_in_tsne", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2], xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()


performKMedoids(KLMatrix, 2, tsne)
performKMedoids(KLMatrix, 3, tsne)
performKMedoids(KLMatrix, 4, tsne)
performKMedoids(KLMatrix, 5, tsne)
performKMedoids(KLMatrix, 6, tsne)
performKMedoids(KLMatrix, 7, tsne)
performKMedoids(KLMatrix, 8, tsne)
performKMedoids(KLMatrix, 9, tsne)
performKMedoids(KLMatrix, 10, tsne)
performKMedoids(KLMatrix, 11, tsne)
performKMedoids(KLMatrix, 12, tsne)
performKMedoids(KLMatrix, 13, tsne)


png("kmedoids_12_details.png", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2], xlim = c(-15,15), ylim = c(-13,12) , main=paste("kmedoids", "k", 12, sep = "_"),
     xlab="Dim1", ylab = "Dim2", col =  adjustcolor(tol12qualitative[kmed$clustering], alpha=0.5), pch=16)
dev.off()

for (cluster in 1:12) {
  png(paste("kmedoids", "cluster", cluster, sep = "_"), width = 1600, height = 1600, res = 300)
  plot(tsne$Y[kmed$clustering == cluster, 1], tsne$Y[kmed$clustering == cluster,2], xlim = c(-15,15), ylim = c(-13,12) ,
       main=paste("kmedoids", "cluster", cluster, sep = "_"), xlab="Dim1", ylab = "Dim2",
       col= adjustcolor(tol12qualitative[cluster], alpha=0.5), pch=16)
  dev.off()
  
  t_cluster = sort(colSums(tm_13[kmed$clustering == cluster, ]), decreasing = TRUE)
  d_cluster = data.frame(word = names(t_cluster),freq=t_cluster)
  
  plotWordCloud(d_cluster, paste("wordcloud", "cluster", cluster, sep = "_"))
}

