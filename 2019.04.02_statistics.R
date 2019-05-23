source('./library_loader.R')

setwd("../Desktop/M1 Internship/Code")
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
#####
#### [DONE] Plotting Wordclouds ####
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

#### [DONE] Plotting the heatmaps with the evolution of thematics ####
m_adj <- as.matrix(utl$Term_Matrix_Adjust(sparsity_thresh = 0.1))
C = cosine(t(m_adj))

png("heatmaps_3to15_tfidf.png", width = 1600, height = 1200, res = 300, pointsize = 8)
heatmap.2(C, main = paste("Equal-Cuts Periods", 3, "to", N, "periods"), labRow = as.character(3:15), labCol = as.character(3:15), dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()


#### [DONE] Perfoming LDA on T1, T3, T6 for 7-cut ####
if (N == 7) {
  ldaOut = LDA(tm,3, method="Gibbs", control=list(nstart=5, seed = list(2003,5,63,100001,765), best=TRUE, burnin = 4000, iter = 2000, thin=500))
  png(paste0("topics_T", 3, ".png"), width = 3200, height = 1800, res = 300)
  termsPerTopic(ldaOut) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    ggtitle("Top 10 words for each discovered topic")+
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  dev.off()
}

#### [DONE - R5] Performing PCA on Covariance Matrix ####
source('./PCA_on_covariance_matrix.R')

#### [DONE - R5] Plotting Users and Terms in Topic Space ####
source("./24.04_LSA_users_and_terms_in_topics_space.R")
source("./25.04_LDA_users_and_terms_in_topics_space.R")

#### [DONE - R5] Polarization of users - t-SNE + plotly####
source('./26.04_TSNE_users_in_words_space.R')
#### [DONE - R5] Clustering ####
source('./26.04_CLUSTERING_users_in_word_space.R')


#### [TODO !!!] Putting two timeframes in the same graphic ####
tm_1 = as.matrix(getTermMatrix(t, 1, 0.98498730956292))
tm_1_uniq = unique(tm_1)
clusters.T1 = applyKMeans(tm_1_uniq, "T1")


tm_14 = as.matrix(getTermMatrix(t, 14, 0.98498730956292))
tm_14_uniq = unique(tm_14)
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


#### [TODO !!!] Transitions ####
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

#### [TODO !!!] Leaders #####

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



