#### Plotting Users in Words Features Space ####
source('./library_loader.R')
source("./utils.R")

# Initialize data
t = initializeData()
PERIOD = 8

T_ = t[[PERIOD]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))
  
tm_ = unique(as.matrix(getTermMatrixWithTM(t, PERIOD, sparsity = 0.93, weightTf)))
tm_ = tm_[rowSums(tm_) > 0,]
dim(tm_)
View(tm_)

cosine.dist = 1 - cosine(t(tm_))
after.pca = cmdscale(cosine.dist, 50)

tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)

png("tsne_tf_cosine_PCA_.png", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="Tf + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()

### Uncomment this line for interactive plots! Warning! Set the sparsity thersh to a value s.t. you don't have too many dimensions.
# plotTSNE(tsne, tm_)
###

### Uncomment this line to run the t-SNE on all time periods
# performTSNE(t, 1:15, 0.98498730956292, F)
###

dbscan.result = performHDBSCAN(cosine.dist, 10, tsne)
plotTSNE(tsne, tm_, adjustcolor(colPal[dbscan.result+1], alpha=0.5))
  
