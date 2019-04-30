#### Plotting Users in Words Features Space ####

tm_13 = as.matrix(getTermMatrixWithTM(t, 13, 0.98498730956292, weightTf))

dim(tm_13)
tm_13_uniq = unique(tm_13)
dim(tm_13_uniq)
tm_13_uniq = tm_13_uniq[rowSums(tm_13_uniq) > 0,]
dim(tm_13_uniq)

cosine.dist = 1 - cosine(t(tm_13_uniq))
after.pca = cmdscale(cosine.dist, 50)

tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)

png("tsne_tf_cosine_PCA_.png", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="Tf + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()

### Uncomment this line for interactive plots! Warning! Set the sparsity thersh to a value s.t. you don't have too many dimensions.
# plotTSNE(tsne, tm_13_uniq[,62:120])
###

### Uncomment this line to run the t-SNE on all time periods
# performTSNE(t, 1:15, 0.98498730956292, F)
###