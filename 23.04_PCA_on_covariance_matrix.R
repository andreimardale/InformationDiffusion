source('./library_loader.R')
library(ggfortify)
library(autoplotly)

source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)
t = splitInTimeframes(posts, 15)

#### Weighting = Tf ####
tm_13 = as.matrix(getTermMatrixWithTM(t, 13, sparsity = 0.98498730956292, weightTf))
tm_uniq_13 = unique(tm_13)
tm_uniq_13 = tm_uniq_13[rowSums(tm_uniq_13) > 0,]
tm_uniq_centered_13 = scale(tm_uniq_13, center = TRUE, scale = FALSE)
dim(tm_uniq_centered_13)

pca_tf_13 <- princomp(tm_uniq_centered_13, cor = FALSE)
png("pca_tf_13_2DRepr.png", width = 1800, height = 1800, res = 300)
autoplot(pca_tf_13)
dev.off()

png("pca_tf_13_variances.png", width = 1800, height = 1800, res = 300)
plot(pca_tf_13, "PCA (T = 13) (Weight = Tf) (centered data + cov matrix)")
dev.off()

png("pca_tf_13_first_components.png", width = 3200, height = 1800, res = 300)
biplot(pca_tf_13, main = "PCA (T = 13) - Tf - Principal Components")
dev.off()

eigs <- pca_tf_13$sdev^2
rbind(SD = sqrt(eigs), Proportion = eigs/sum(eigs), Cumulative = cumsum(eigs)/sum(eigs))[,1:10]

pc = as.data.frame(pca_tf_13$scores[,1:2])
View(pc)

colors = rep(1, dim(pc)[1])
colors[1] = 2
colors[9] = 3
colors[2] = 4
colors[10] = 5
colors[3] = 6
colors[24] = 7
colors[12] = 8
fin_cols = adjustcolor(colors, alpha=0.2)
fin_cols[1] = adjustcolor(colors[1], alpha = 1)
fin_cols[9] = adjustcolor(colors[9], alpha = 1)
fin_cols[2] = adjustcolor(colors[2], alpha = 1)
fin_cols[10] = adjustcolor(colors[10], alpha = 1)
fin_cols[3] = adjustcolor(colors[3], alpha = 1)
fin_cols[24] = adjustcolor(colors[24], alpha = 1)
fin_cols[12] = adjustcolor(colors[12], alpha = 1)


png("pca_representation.png", width = 1800, height = 1800, res = 300)
plot(pc, main = "PCA (T = 13) (Weight = Tf) (centered data + cov matrix)", col = fin_cols, pch = 16)
dev.off()


cosine.dist = 1 - cosine(t(tm_uniq_13))
tsne = Rtsne(cosine.dist, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = T, pca = F)

png("tsne_plot.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2], main="Tf + Cosine + NoPCA + tSNE",xlab="Dim1", ylab = "Dim2", col = fin_cols, pch=16)
dev.off()


#### Weighting = Tf-Idf ####
tm_13 = as.matrix(getTermMatrix(t, 13, sparsity = 0.98498730956292, T))
tm_uniq_13 = unique(tm_13)
dim(tm_uniq_13)
tm_uniq_centered_13 = scale(tm_uniq_13, center = TRUE, scale = FALSE)

pca_tf_13 <- princomp(tm_uniq_centered_13, cor = FALSE)
png("pca_tfidf_13_2DRepr.png", width = 1800, height = 1800, res = 300)
autoplot(pca_tf_13)
dev.off()

png("pca_tfidf_13_variances.png", width = 1800, height = 1800, res = 300)
plot(pca_tf_13, "PCA (T = 13) (Weight = TfIdf) (centered data + cov matrix)")
dev.off()

png("pca_tfidf_13_first_components.png", width = 3200, height = 1800, res = 300)
biplot(pca_tf_13, main = "PCA (T = 13) - TfIdf - Principal Components")
dev.off()

eigs <- pca_tf_13$sdev^2
rbind(SD = sqrt(eigs), Proportion = eigs/sum(eigs), Cumulative = cumsum(eigs)/sum(eigs))[,1:10]