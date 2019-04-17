library(gtools)
library(ggplot2)
library(gplots)
library(plotly)

library(data.tree)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library(topicmodels)
library(tidyr)
library(tidytext)
library(dplyr)

library(Rtsne)
library(ClusterR)
library(dbscan)

library(lsa)

setwd("../Desktop/M1 Internship/Code")
source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)

N = 10
# Split the posts into a list of timeframes defined by the array of cutting points inside this function.
t = splitInTimeframes(posts, N, T)

#### Plotting Wordclouds ####
content <- character(0)
for (i in 1:N) {
  content = c(content, paste(unlist(t[[i]]$Content), collapse =" "))
}

utl = textTinyR::sparse_term_matrix$new(vector_data = content, file_data = NULL, document_term_matrix = TRUE)

tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = T,
                     remove_numbers = T, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)
for (i in 1:N) {
  plotWordCloud(getMostFrequentWords(tm, i), print(paste0(paste("timeframe", i, sep="_"), ".png")))
}
plotWordCloud(getOverallMostFrequentWords(tm), "timeframe_all.png")

#### Plotting the heatmaps with the evolution of thematics ####
m_adj <- as.matrix(utl$Term_Matrix_Adjust(sparsity_thresh = 0.1))
#C = getCosineSimilarity(m_adj)
C = cosine(t(m_adj))


png("heatmaps.png", width = 1600, height = 1200, res = 300, pointsize = 8)
heatmap.2(C, main = paste("Cos similarity between the", N, "periods"), dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()


#### Perfoming LDA on T1, T3, T6 for 7-cut ####
if (N == 7) {
  ldaOut = LDA(tm,3, method="Gibbs", control=list(nstart=5, seed = list(2003,5,63,100001,765), best=TRUE, burnin = 4000, iter = 2000, thin=500))
  ap_topics <- tidy(ldaOut, matrix = "beta")
  ap_topics
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  png("topics_T3.png", width = 3200, height = 1800, res = 300)
  ap_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  dev.off()
}

#### Polarization of users - t-SNE + plotly####
performTSNE(t, 5:5, lseq(0.97, 1, 7))

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

#### Uncomment for interactive view of the t-SNE ####
# T_ext_by_authors = t[[1]] %>%
#   group_by(Author) %>%
#   summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
#   arrange(desc(nr_of_posts))
# 
# init = textTinyR::sparse_term_matrix$new(vector_data = T_ext_by_authors$text, file_data = NULL, document_term_matrix = TRUE)
# 
# tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
#                       remove_numbers = T, trim_token = T, split_string = T, 
#                       stemmer = "porter2_stemmer",
#                       split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
#                       language = "english", min_num_char = 3, max_num_char = 100,
#                       print_every_rows = 100000, normalize = NULL, tf_idf = T, 
#                       threads = 6, verbose = T)
# m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 1))
# m_adj_uniq = unique(m_adj)
# dim(m_adj_uniq)
# 
# tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000)
# plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 30",xlab="Dim1", ylab = "Dim2")
# plotTSNE(tsne, scale(m_adj_uniq))
# 
# library(rgl)
# plot3d(x=tsne$Y[,1],y=tsne$Y[,2],z=tsne$Y[,3],
#        type="s",radius=0.5)
###

#### Clustering ####
scal_dat = ClusterR::center_scale(m_adj_uniq)
kmed = ClusterR::Cluster_Medoids(scal_dat, clusters = 2, 
                                 distance_metric = "pearson_correlation",
                                 minkowski_p = 1, threads = 6, swap_phase = TRUE, 
                                 fuzzy = FALSE, verbose = F, seed = 1)

t1 = sort(colSums(m_adj_uniq[kmed$clusters == 1, ]), decreasing = TRUE)
d1 = data.frame(word = names(t1),freq=t1)

t2 = sort(colSums(m_adj_uniq[kmed$clusters == 2, ]), decreasing = TRUE)
d2 = data.frame(word = names(t2),freq=t2)

plotWordCloud(d1, "cluster1.png")
plotWordCloud(d2, "cluster2.png")

png("kmedoids_tsne_t2.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="2D Representation - T2 (16-cut); perplexity = 30",xlab="Dim1", ylab = "Dim2", col = kmed$clusters)
dev.off()

#### Putting two timeframes in the same graphic ####

T1 = t[[1]] %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

tm_1_uniq = getTermMatrix(T1, 0.98498730956292)
clusters.T1 = applyKMeans(tm_1_uniq, "T1")


T14 = t[[14]] %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

tm_14_uniq = getTermMatrix(T14, 0.98498730956292)
clusters.T14 = applyKMeans(tm_14_uniq, "T14")

rez = smartbind(tm_1_uniq, tm_14_uniq)
rez[is.na(rez)] <- 0
rez = unique(rez)
dim(rez)

tsne = Rtsne(scale(rez),  dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000)

colors = c(rep(1, dim(tm_1_uniq)[1]), rep(2, dim(tm_14_uniq)[1]))
png("tsne_T1_T14.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[,1], tsne$Y[,2],main="T1 vs T14; perplexity = 50",xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.25), pch=16)
legend("topleft",c("T1","T14"), cex=.8, col=c("black","red"),pch=c(16,16))
dev.off()

png("tsne_T1.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[1:dim(tm_1_uniq)[1],1], tsne$Y[1:dim(tm_1_uniq)[1], 2],main="T1; perplexity = 50",xlab="Dim1", ylab = "Dim2", col = adjustcolor(clusters.T1 + 2, alpha=0.5), pch=16)
legend("topleft",c("Cluster 1","Cluster 2"), cex=.8, col=c("green","blue"),pch=c(16,16))
dev.off()

png("tsne_T14.png", width = 1800, height = 1800, res = 300)
plot(tsne$Y[dim(tm_1_uniq)[1]:dim(rez)[1],1], tsne$Y[dim(tm_1_uniq)[1]:dim(rez)[1], 2],main="T14; perplexity = 50",xlab="Dim1", ylab = "Dim2", col = adjustcolor(clusters.T14 + 5, alpha=0.5), pch=16)
legend("topleft",c("Cluster 1","Cluster 2"), cex=.8, col=c("purple","yellow"),pch=c(16,16))
dev.off()


####################
common_authors = intersect(T1$Author, T14$Author)
T1$index = rownames(T1)
T14$index = rownames(T14)

init = textTinyR::sparse_term_matrix$new(vector_data = T1$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T, 
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                      threads = 6, verbose = T)
tm_1 <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
print(paste0("Dimension of the term matrix ", paste(dim(tm_1)[1], dim(tm_1)[2], sep = ",") ))

init = textTinyR::sparse_term_matrix$new(vector_data = T14$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T, 
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                      threads = 6, verbose = T)
tm_14 <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))
print(paste0("Dimension of the term matrix ", paste(dim(tm_14)[1], dim(tm_14)[2], sep = ",") ))

for (auth in 1:21) {

  index.T1 = subset(T1, T1$Author == common_authors[auth], select = c("index"))
  index.T14 = subset(T14, T14$Author == common_authors[auth], select = c("index"))
  
  getTransition(auth, tm_1, tm_14)
    
}

#### Leaders #####

png("tsne_T1_T14_50_leaders.png", width = 1800, height = 1800, res = 300)
colors = c(rep(1, dim(tm_1_uniq)[1]), rep(2, dim(tm_14_uniq)[1]))
plot(tsne$Y[,1], tsne$Y[,2],main="T1 vs T14 - Leaders of T1 and T14; perplexity = 50",xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.25), pch=16)

init = textTinyR::sparse_term_matrix$new(vector_data = T1$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T,
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = T,
                      threads = 6, verbose = T)
tm_1 <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))

init = textTinyR::sparse_term_matrix$new(vector_data = T14$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T, 
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                      threads = 6, verbose = T)
tm_14 <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.98498730956292))


for (authorNumber in 1:50) {
  plotLeaders(authorNumber, tm_1, "T1")
  plotLeaders(authorNumber, tm_14, "T14")
}

legend("topleft",c("T1","T14", "Leader T1", "Leader T14"), cex=.8, col=c("black","red", "green", "blue"),pch=c(16,16, 1, 1))
dev.off()

##### Analysis ####
png("hist.png", width = 3200, height = 1800, res = 300)
hist(posts$Date, "months", format = "%b-%y")
dev.off()

dim(diffusions_comments)
dim(diffusions_submissions)


# Unique Users
unique.authors.submission = diffusions_submissions %>%
  group_by(Author) %>%
  summarise( nr_of_submissions = n(), mean_score = mean(Score)) %>%
  arrange(desc(nr_of_submissions)) %>%
  head(n=100)

unique.authors.comments = diffusions_comments %>%
  group_by(Author) %>%
  summarise( nr_of_comments = n(), mean_score = mean(Score)) %>%
  arrange(desc(nr_of_comments)) %>%
  head(n=100)

unique.authors = posts %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), mean_score = mean(Score)) %>%
  arrange(desc(nr_of_posts))

diffusions = diffusions_comments %>%
  group_by(Submission.ID) %>%
  summarise( nr_of_messages = n())
hist(diffusions$nr_of_messages, breaks = 50)

comments.vs.submissions = diffusions_submissions %>%
  group_by(NofComments) %>%
  summarise(nr_of_submissions = n())
ggplot(data=comments.vs.submissions, aes(comments.vs.submissions$NofComments,comments.vs.submissions$nr_of_submissions) ) +
  geom_line() +
  geom_point(color="red") + labs(x = "Number of messages") + labs(y = "No of Submissions")



no_of_messages_ecdf <- ecdf( diffusions_submissions$NofComments )
# now put the ecdf and its complementary in a data.frame
df <- data.frame( x = sort(diffusions_submissions$NofComments),
                  y = 1-no_of_messages_ecdf(sort(diffusions_submissions$NofComments) ))

df_log <- data.frame( x = log(sort(diffusions_submissions$NofComments)),
                      y = log(1-no_of_messages_ecdf(sort(diffusions_submissions$NofComments) )))

# plot
ggplot(data=df, aes(x, y) ) +
  geom_line() +
  geom_point(color="red") + labs(x = "Number of messages") + labs(y = "Probability")

ggplot(data=df_log, aes(x, y) ) +
  geom_line() +
  geom_point(color="blue")+ labs(x = "log(Number of messages)") + labs(y = "log(Probability)")

###################
score_ecdf <- ecdf( diffusions_submissions$Score )

# now put the ecdf and its complementary in a data.frame
df <- data.frame( x = log(sort(diffusions_submissions$Score)),
                  y = log(1-score_ecdf(sort(diffusions_submissions$Score) )))

# plot
ggplot(data=df, aes(x, y) ) +
  geom_line() +
  geom_point(color="red") + labs(x = "Score") + labs(y = "Probability")

###################
#Different time frames#

getCCDFTimestamps <- function(posts) {
  submissions = subset(posts, is.na(posts$Comment.ID))
  
  no_of_messages_ecdf <- ecdf( submissions$NofComments )
  
  # now put the ecdf and its complementary in a data.frame
  df_log <- data.frame( x = log(sort(submissions$NofComments)),
                        y = log(1-no_of_messages_ecdf(sort(submissions$NofComments) )))
  return(df_log)
}

d1 = getCCDFTimestamps(posts.T1)
d2 = getCCDFTimestamps(posts.T2)
d3 = getCCDFTimestamps(posts.T3)
d4 = getCCDFTimestamps(posts.T4)

ggplot() + geom_line(aes(x=d1$x,y=d1$y, color='red')) + 
  geom_line(aes(x=d2$x,y=d2$y,color='green')) + 
  geom_line(aes(x=d3$x,y=d3$y,color='blue')) + 
  geom_line(aes(x=d4$x,y=d4$y,color='purple')) + 
  ylab('log(Probability)')+xlab('log(NoOfMessages)') +
  scale_color_discrete(name = "CCDF", labels = c("T1", "T2", "T3", "T4"))


#buildTree("b9o5ci")



