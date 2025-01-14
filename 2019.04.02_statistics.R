source('./library_loader.R')
source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)

# The number of time-frames that the data will be split into
N = 15

# Split the posts into a list of timeframes defined by the array of cutting points inside this function.
t = splitInTimeframes(posts, N)

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
C = cosine(t(m_adj))

png("heatmaps_3to15_tfidf.png", width = 1600, height = 1200, res = 300, pointsize = 8)
heatmap.2(C, main = paste("Event-based Periods"), labRow = as.character(1:15), labCol = as.character(1:15), dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()


#### Performing PCA on Covariance Matrix ####
source('./2019.04.23_PCA_on_covariance_matrix.R')

#### Plotting Users and Terms in Topic Space ####
source("./2019.04.25_LDA_users_and_terms_in_topics_space.R")

#### Plotting Users in Words Features Space - t-SNE + plotly####
source('./2019.04.26_TSNE_users_in_words_space.R')

#### Clustering of users in Words Space ####
source('./2019.04.26_CLUSTERING_users_in_word_space.R')


#### Putting two timeframes in the same graphic ####
tm_1_uniq = unique(as.matrix(getTermMatrixWithTM(t, 1, 0.98498730956292)))
clusters.T1 = applyKMeans(tm_1_uniq, "T1")

tm_14_uniq = unique(as.matrix(getTermMatrixWithTM(t, 14, 0.98498730956292)))
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


#### Transitions between different time periods ####
T1 = t[[1]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

T14 = t[[14]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

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

for (auth in 1:2) {

  index.T1 = subset(T1, T1$Author == common_authors[auth], select = c("index"))
  index.T14 = subset(T14, T14$Author == common_authors[auth], select = c("index"))
  
  getTransition(auth, tm_1, tm_14)
}


#### Find and Plot the Leaders of two different time periods. (leader = user with high number of posts) #####
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


for (authorNumber in 1:2) {
  plotLeaders(authorNumber, tm_1, "T1")
  plotLeaders(authorNumber, tm_14, "T14")
}

legend("topleft",c("T1","T14", "Leader T1", "Leader T14"), cex=.8, col=c("black","red", "green", "blue"),pch=c(16,16, 1, 1))
dev.off()


##### Structural Analysis ####

png("histogram_Of_Posts.png", width = 3200, height = 1800, res = 300)
hist(posts$Date, "days", format = "%b-%y", main = NULL, xlab = "Date", ylab = "Density of Posts")
dev.off()

dim(diffusions_comments)[1]
dim(diffusions_submissions)[1]

png("density_Of_Posts.png", width = 3200, height = 1800, res = 300)
pts_N_cut = c(as.Date("2016-06-23", format="%Y-%m-%d"), as.Date("2018-07-08", format="%Y-%m-%d"),
              as.Date("2018-11-14", format="%Y-%m-%d"), as.Date("2019-03-29", format="%Y-%m-%d"))
ggplot(posts, aes(Date)) +
  geom_density(bw = 5) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pts_N_cut)

rm(pts_N_cut)

dev.off()

# Unique Users
unique.authors.submission = diffusions_submissions %>%
  group_by(Author) %>%
  summarise( nr_of_submissions = n(), mean_score = mean(Score)) %>%
  arrange(desc(nr_of_submissions))
dim(unique.authors.submission)

unique.authors.comments = diffusions_comments %>%
  group_by(Author) %>%
  summarise( nr_of_comments = n(), mean_score = mean(Score)) %>%
  arrange(desc(nr_of_comments))
dim(unique.authors.comments)

common <- intersect(unique.authors.submission$Author, unique.authors.comments$Author)
both = length(common)
initiatiators = dim(unique.authors.submission)[1] - both
commentors = dim(unique.authors.comments)[1] - both


unique.authors = posts %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), mean_score = mean(Score)) %>%
  arrange(desc(nr_of_posts))
dim(unique.authors)


png("Structure.png", width = 3200, height = 1800, res = 300)
par(mfrow=c(1,2))
slices <- c(dim(diffusions_submissions)[1], dim(diffusions_comments)[1])
lbls <- c("Threads", "Comments")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, "\n(", pct) # add percents to labels 
lbls <- paste(lbls,"%)",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)))

slices <- c(initiatiators, commentors, both)
lbls <- c("Thread\n Initiators", "Commenters", "Both")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, "\n(", pct) # add percents to labels 
lbls <- paste(lbls,"%)",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)))

dev.off()

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
df <- data.frame( x = sort(diffusions_submissions$NofComments),
                  y = 1-no_of_messages_ecdf(sort(diffusions_submissions$NofComments) ))
  
df_log <- data.frame( x = log(sort(diffusions_submissions$NofComments)),
                      y = log(1-no_of_messages_ecdf(sort(diffusions_submissions$NofComments) )))

# plot
ggplot(data=df, aes(x, y) ) +
  geom_line() +
  geom_point(color="red") + labs(x = "Number of messages") + labs(y = "Probability")

png("ccdf_noMess_prob.png", width = 3200, height = 1800, res = 300)
ggplot(data=df_log, aes(x, y) ) +
  geom_line() +
  geom_point(color="blue")+ labs(x = "log(Number of messages)") + labs(y = "log(Probability)")

dev.off()
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

# Different time frames #

getCCDFTimestamps <- function(posts) {
  submissions = subset(posts, is.na(posts$Comment.ID))
  
  no_of_messages_ecdf <- ecdf( submissions$NofComments )
  
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
