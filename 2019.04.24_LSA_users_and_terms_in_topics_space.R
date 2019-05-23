source('./library_loader.R')

source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)
t = splitInTimeframes(posts, 15)

#### Plotting Users and Terms in Topic Space ####

tm_13 = as.matrix(getTermMatrix(t, 13, sparsity = 0.98498730956292, T))
tm_uniq_13 = unique(tm_13)
dim(tm_uniq_13)

lsa_13 = lsa(tm_uniq_13, dims=dimcalc_share())
U_13 = lsa_13$tk
T_13 = lsa_13$dk
S_13 = diag(length(lsa_13$sk)) * lsa_13$sk

dim(U_13)
dim(T_13)
dim(S_13)

tm_k_rebuilt = U_13 %*% S_13 %*% t(T_13)
dim(tm_k_rebuilt)

err = frobenius.norm(tm_uniq_13 - tm_k_rebuilt)

#### 

A = rbind(U_13, T_13)

A_norm = t(apply(A, 1, function(x)(x-min(x))/(max(x)-min(x))))
A_norm_2 = A_norm / rowSums(A_norm)

KLMatrix <- KL(A_norm_2)

cosine.dist = 1 - cosine(t(A))
after.pca = cmdscale(cosine.dist, 50)

colors = c(rep(1, dim(U_13)[1]), rep(2, dim(T_13)[1]))
tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)

png("tsne_topcs_with_PCA_tfidf_cosine.png", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="LSA + KL + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
dev.off()

getText <- function() {
  content <- character(0)
  for (i in 1 : dim(U_13)[1]) {
    content[i] = as.character(i)
  }
  
  content = c(content, colnames(tm_uniq_13))
  
  return(content)
}

hover_text <- getText()

plotdata <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2],
                       hover_text = hover_text)
plt2 <- ggplot(plotdata) + 
  geom_point(aes(x = tsne_x, y = tsne_y, text = hover_text), color = adjustcolor(colors, alpha=0.5))

ggplotly(plt2, width = 3200, height = 1600, res = 300)