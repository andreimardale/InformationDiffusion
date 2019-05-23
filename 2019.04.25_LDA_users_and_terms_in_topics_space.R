#### Plotting Users and Terms in Topic Space ###
source('./library_loader.R')
source("./utils.R")

# Initialize data
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

rez = topicmodels::posterior(model)
phi = rez$terms
theta = rez$topics
gamma = t(CalcGamma(phi, theta, p_docs = Matrix::rowSums(tm_)))

A = rbind(theta, gamma)
KLMatrix <- KL(A)


colors = c(rep(1, dim(theta)[1]), rep(2, dim(gamma)[1]))

tsne = Rtsne(KLMatrix, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)
png(paste0(paste("lda", K, "tsne_NoPCA.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
par(mfrow=c(1,2))
plot(tsne$Y[,1], tsne$Y[,2],main=paste0(K,"_LDA + KL + NoPCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
legend("topleft",c("Users","Terms"), cex=.8, col=c("black","red"),pch=c(16,16))
plot(tsne$Y[1: dim(theta)[1],1], tsne$Y[1: dim(theta)[1],2],main=paste0(K,"_LDA + KL + NoPCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
legend("topleft",c("Users"), cex=.8, col=c("black"),pch=c(16))
dev.off()

### Use PCA before performing t-SNE ####
# after.pca = cmdscale(KLMatrix, 50)
# tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
# png(paste0(paste("lda", K, "tsne_PCA.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
# par(mfrow=c(1,2))
# plot(tsne$Y[,1], tsne$Y[,2],main=paste0(K,"_LDA + KL + PCA + tSNE"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
# legend("topleft",c("Users","Terms"), cex=.8, col=c("black","red"),pch=c(16,16))
# plot(tsne$Y[1: dim(theta)[1],1], tsne$Y[1: dim(theta)[1],2],main=paste0(K,"_LDA + KL + PCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
# legend("topleft",c("Users"), cex=.8, col=c("black"),pch=c(16))
# dev.off()
####

### Uncomment to plot interactively ####
hover_text <- character(0)
for (i in 1 : dim(theta)[1]) {
  hover_text[i] = substr(as.character(T_[i, "text"]), 1, 200)
}
hover_text = c(hover_text, colnames(tm_))

plotdata <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2], hover_text = hover_text)
plt2 <- ggplot(plotdata) + geom_point(aes(x = tsne_x, y = tsne_y, text = hover_text), color = adjustcolor(colors, alpha=0.5))
ggplotly(plt2)
###




