# Tf-Idf+Scale(columns) + tSNE(Euclid+PCA)
v1 <- function(t) {
  T13 = t[[13]] %>%
    group_by(Author) %>%
    summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(desc(nr_of_posts))
  
  init = textTinyR::sparse_term_matrix$new(vector_data = T13$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = TRUE, to_lower = T, remove_punctuation_vector = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                        threads = 6, verbose = T)
  
  m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
  dim(m_adj)
  m_adj_uniq = unique(m_adj)
  dim(m_adj_uniq)
  
  tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000)
  png("tsne_v1.png", width = 1800, height = 1800, res = 300)
  plot(tsne$Y[,1], tsne$Y[,2],main="Tf-Idf+Scale(columns) + tSNE(Euclid+PCA)",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  dev.off()
}

# v2: Tf + Scale + tSNE(Euclid+PCA)
# v2_2: Tf + tSNE(Euclid+PCA)
v2 <- function(t) {
  T13 = t[[13]] %>%
    group_by(Author) %>%
    summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(desc(nr_of_posts))
  
  init = textTinyR::sparse_term_matrix$new(vector_data = T13$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = TRUE, to_lower = T, remove_punctuation_vector = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                        threads = 6, verbose = T)
  
  m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
  dim(m_adj)
  m_adj_uniq = unique(m_adj)
  dim(m_adj_uniq)
  
  tsne = Rtsne(m_adj_uniq,  dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000)
  tsne_scale = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000)
  
  png("tsne_v2.png", width = 3200, height = 1600, res = 300)
  par(mfrow=c(1,2))
  plot(tsne$Y[,1], tsne$Y[,2],main="Tf + tSNE(Euclid+PCA)",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  plot(tsne_scale$Y[,1], tsne_scale$Y[,2],main="Tf + Scale + tSNE(Euclid+PCA)",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  dev.off()
  
}

v3 <- function(t) {
  T13 = t[[13]] %>%
    group_by(Author) %>%
    summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(desc(nr_of_posts))
  
  init = textTinyR::sparse_term_matrix$new(vector_data = T13$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = TRUE, to_lower = T, remove_punctuation_vector = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                        threads = 6, verbose = T)
  
  m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
  dim(m_adj)
  m_adj_uniq = unique(m_adj)
  dim(m_adj_uniq)
  
  euclidean_scale = dist(scale(m_adj_uniq), method = "euclidean")
  euclidean = dist(m_adj_uniq, method = "euclidean")
  
  pc_scale = cmdscale(euclidean_scale, k = 50)
  pc = cmdscale(euclidean, k = 50)
  
  tsne_scale = Rtsne(pc_scale, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  tsne = Rtsne(pc, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  
  png("tsne_v3.png", width = 3200, height = 1600, res = 300)
  par(mfrow=c(1,2))
  plot(tsne$Y[,1], tsne$Y[,2],main="Tf + Euclidean + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  plot(tsne_scale$Y[,1], tsne_scale$Y[,2],main="Tf + Scale + Euclidean + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  dev.off()
}

v4 <- function(t) {
  T13 = t[[13]] %>%
    group_by(Author) %>%
    summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(desc(nr_of_posts))
  
  init = textTinyR::sparse_term_matrix$new(vector_data = T13$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = TRUE, to_lower = T, remove_punctuation_vector = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                        threads = 6, verbose = T)
  
  m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.91))
  dim(m_adj)
  m_adj_uniq = unique(m_adj)
  dim(m_adj_uniq)
  m_adj_uniq = m_adj_uniq[rowSums(m_adj_uniq) > 0,]
  dim(m_adj_uniq)
  
  
  cosine_scale = cosine(t(normalize_input(m_adj_uniq)))
  cosine = cosine(t(m_adj_uniq))
  
  pc_scale = cmdscale(cosine_scale, k = 50)
  pc = cmdscale(cosine, k = 50)
  
  tsne_scale = Rtsne(pc_scale, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  tsne = Rtsne(pc, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  
  png("tsne_v4.png", width = 3200, height = 1600, res = 300)
  par(mfrow=c(1,2))
  plot(tsne$Y[,1], tsne$Y[,2],main="Tf + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  plot(tsne_scale$Y[,1], tsne_scale$Y[,2],main="Tf + Scale + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  dev.off()
}

v5 <- function(t) {
  T13 = t[[13]] %>%
    group_by(Author) %>%
    summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(desc(nr_of_posts))
  
  init = textTinyR::sparse_term_matrix$new(vector_data = T13$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = TRUE, to_lower = T, remove_punctuation_vector = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                        threads = 6, verbose = T)
  
  m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.9))
  dim(m_adj)
  m_adj_uniq = unique(m_adj)
  dim(m_adj_uniq)
  m_adj_uniq = m_adj_uniq[rowSums(m_adj_uniq) > 0,]
  dim(m_adj_uniq)
  
  
  cosine_scale = cosine(t(scale(m_adj_uniq)))
  cosine = cosine(t(m_adj_uniq))
  
  pc_scale = cmdscale(cosine_scale, k = 50)
  pc = cmdscale(cosine, k = 50)
  
  tsne_scale = Rtsne(pc_scale, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  tsne = Rtsne(pc, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
  
  png("tsne_v5.png", width = 3200, height = 1600, res = 300)
  par(mfrow=c(1,2))
  plot(tsne$Y[,1], tsne$Y[,2],main="Tf-Idf + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  plot(tsne_scale$Y[,1], tsne_scale$Y[,2],main="Tf+Idf + Scale + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
  dev.off()
}


T13 = t[[13]] %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

init = textTinyR::sparse_term_matrix$new(vector_data = T13$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T, 
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                      threads = 3, verbose = T)

m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
dim(m_adj)
m_adj_uniq = unique(m_adj)
dim(m_adj_uniq)
m_adj_uniq = m_adj_uniq[rowSums(m_adj_uniq) > 0,]
dim(m_adj_uniq)

cosine.dist = 1 - cosine(t(m_adj_uniq))
after.pca = cmdscale(cosine.dist, 50)

tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)

png("tsne_tf_dist_PCA_1463words.png", width = 1600, height = 1600, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="Tf + Cosine + PCA + tSNE",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()

plotTSNE(tsne, m_adj_uniq[,1:39])
performTSNE(t, 13, 0.9825932, T)











#### Before ####

T_ = t[[13]] %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

init = textTinyR::sparse_term_matrix$new(vector_data = T_$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T, 
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                      threads = 6, verbose = T)

m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
dim(m_adj)
m_adj = unique(m_adj)
dim(m_adj)


#euclidean_sim <- dist(scale(m_adj), method = "euclidean")

m_adj = m_adj[rowSums(m_adj) > 0,]

#pc = cmdscale(dist(scale(m_adj), method = "euclidean"), k = 50)

pc_cosine = cmdscale(cosine(t(scale(m_adj))), k = 50)

#cosine_similarity_uniq = unique(t(unique(cosine_sim)))

tsne = Rtsne(pp$x, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
png("tsne.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="2D Representation - T1 (15-cut); perplexity = 5",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()

tsne = Rtsne(cosine_similarity_uniq,  dims = 2, perplexity=30, verbose=TRUE, max_iter = 3000)
png("tsne_30.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="2D Representation - T1 (15-cut); perplexity = 30",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()

tsne = Rtsne(cosine_similarity_uniq,  dims = 2, perplexity=50, verbose=TRUE, max_iter = 3000)
png("tsne_50.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="2D Representation - T1 (15-cut); perplexity = 50",xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
dev.off()


####



