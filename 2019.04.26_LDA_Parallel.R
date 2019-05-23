source('./library_loader.R')
source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)

N = 15

# Split the posts into a list of timeframes defined by the array of cutting points inside this function.
t = splitInTimeframes(posts, N)

tm_13 = as.matrix(getTermMatrixWithTM(t, 13, sparsity = 0.98498730956292, weightTf))
dim(tm_13)
tm_13 = unique(tm_13)
dim(tm_13)
tm_13 = tm_13[rowSums(as.matrix(tm_13)) > 0,]

k_list <- seq(2,3, by = 1)
set.seed(12345)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  
  model <- FitLdaModel(dtm = tm_13, k = k, iterations = 2000, burnin = 180, alpha = 0.1, beta = 0.05,
                       optimize_alpha = TRUE, calc_likelihood = TRUE, calc_r2 = TRUE, cpus = 3)
  
  
  png(paste0(paste("lda", k, "loglikelihood.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
  plot(model$log_likelihood, type="l", main = paste("Log Likelihood - R2 score", model$r2, sep = " "))
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
  
  
  png(paste0(paste("lda", k, "topics.", sep = "_"), "png"), width = 3200, height = 1800, res = 300)
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
  png(paste0(paste("lda", k, "tsne_NoPCA.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
  par(mfrow=c(1,2))
  plot(tsne$Y[,1], tsne$Y[,2],main=paste0(k,"_LDA + KL + NoPCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
  legend("topleft",c("Users","Terms"), cex=.8, col=c("black","red"),pch=c(16,16))
  plot(tsne$Y[1: dim(model$theta)[1],1], tsne$Y[1: dim(model$theta)[1],2],main=paste0(k,"_LDA + KL + NoPCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
  legend("topleft",c("Users"), cex=.8, col=c("black"),pch=c(16))
  dev.off()
  
  tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  png(paste0(paste("lda", k, "tsne_PCA.", sep = "_"), "png"), width = 3200, height = 1600, res = 300)
  par(mfrow=c(1,2))
  plot(tsne$Y[,1], tsne$Y[,2],main=paste0(k,"_LDA + KL + PCA + tSNE"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
  legend("topleft",c("Users","Terms"), cex=.8, col=c("black","red"),pch=c(16,16))
  plot(tsne$Y[1: dim(model$theta)[1],1], tsne$Y[1: dim(model$theta)[1],2],main=paste0(k,"_LDA + KL + PCA + tSNE"),xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.5), pch=16)
  legend("topleft",c("Users"), cex=.8, col=c("black"),pch=c(16))
  dev.off()
  
}, cpus = 2) 
