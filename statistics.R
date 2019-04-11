library(gtools)
library(ggplot2)
library(gplots)
library(plotly)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library(topicmodels)
library(tidyr)
library(tidytext)
library(dplyr)

library(tsne)
library(Rtsne)

setwd("../Desktop/M1 Internship/Code")
source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)

#### Plotting Wordclouds ####
wd = c(as.Date("2015-11-16", format="%Y-%m-%d"), as.Date("2016-06-24", format="%Y-%m-%d"),
       as.Date("2017-03-30", format="%Y-%m-%d"), as.Date("2018-07-07", format="%Y-%m-%d"),
       as.Date("2018-11-26", format="%Y-%m-%d"), as.Date("2019-03-21", format="%Y-%m-%d"),
       as.Date("2019-03-30", format="%Y-%m-%d"), as.Date("2019-04-06", format="%Y-%m-%d"))

rez = cut.Date(posts$Date, breaks=wd, labels = c("T1", "T2", "T3", "T4", "T5", "T6", "T7"))
summary(rez)
t = split(posts, rez)

T1.content = paste(unlist(t[[1]]$Content), collapse =" ")
T2.content = paste(unlist(t[[2]]$Content), collapse =" ")
T3.content = paste(unlist(t[[3]]$Content), collapse =" ")
T4.content = paste(unlist(t[[4]]$Content), collapse =" ")
T5.content = paste(unlist(t[[5]]$Content), collapse =" ")
T6.content = paste(unlist(t[[6]]$Content), collapse =" ")
T7.content = paste(unlist(t[[7]]$Content), collapse =" ")


content = c(T1.content, T2.content, T3.content, T4.content, T5.content, T6.content, T7.content)

utl = textTinyR::sparse_term_matrix$new(vector_data = content, file_data = NULL, document_term_matrix = TRUE)

tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = T,
                     remove_numbers = T, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)

plotWordCloud(getMostFrequentWords(tm, 1), "timeframe_1.png")
plotWordCloud(getMostFrequentWords(tm, 2), "timeframe_2.png")
plotWordCloud(getMostFrequentWords(tm, 3), "timeframe_3.png")
plotWordCloud(getMostFrequentWords(tm, 4), "timeframe_4.png")
plotWordCloud(getMostFrequentWords(tm, 5), "timeframe_5.png")
plotWordCloud(getMostFrequentWords(tm, 6), "timeframe_6.png")
plotWordCloud(getMostFrequentWords(tm, 7), "timeframe_7.png")

d_overall = getOverallMostFrequentWords(tm)
plotWordCloud(d_overall, "timeframe_all.png")

#### Plotting the heatmaps with the evolution of thematics ####

#Removing sparse terms
m_adj <- as.matrix(utl$Term_Matrix_Adjust(sparsity_thresh = 0.1))
C = getCosineSimilarity(m_adj)

png("heatmaps.png", width = 1600, height = 1200, res = 300, pointsize = 8)
heatmap.2(C, main = "Cos. similarity between the 7 periods", dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()

#### Perfoming LDA on T1, T3, T6 for 7-cut ####
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




##### Polarization of users - t-SNE + plotly####

performTSNE(t)

dim(m_adj_uniq)

T_ext_by_authors = t[[1]] %>%
  group_by(Author) %>%
  summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

init = textTinyR::sparse_term_matrix$new(vector_data = T_ext_by_authors$text, file_data = NULL, document_term_matrix = TRUE)

tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                      remove_numbers = T, trim_token = T, split_string = T, 
                      stemmer = "porter2_stemmer",
                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                      language = "english", min_num_char = 3, max_num_char = 100,
                      print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                      threads = 6, verbose = T)
m = as.matrix(tm)
m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = 0.9))
m_adj_uniq = unique(m_adj)
dim(m_adj_uniq)

tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=5, verbose=TRUE, max_iter = 2000)
plotTSNE(tsne, scale(m_adj_uniq))
#plot(tsne$Y[,1], tsne$Y[,2])

png("T1_6807_words_embedd.png", width = 3200, height = 1800, res = 300)
par(mfrow=c(1,3))
tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=5, verbose=TRUE, max_iter = 2000)
plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 5",xlab="Dim1", ylab = "Dim2")

tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000)
plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 30",xlab="Dim1", ylab = "Dim2")

tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000)
plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 50",xlab="Dim1", ylab = "Dim2")
dev.off()


library(rgl)
plot3d(x=tsne$Y[,1],y=tsne$Y[,2],z=tsne$Y[,3],
       type="s",radius=0.5)


##### Analysis ####
png("hist.png", width = 3200, height = 1800, res = 300)
hist(posts$Date, "months", format = "%b-%y")
dev.off()

dim(diffusions_comments)
dim(diffusions_submissions)


# Unique Users
library(dplyr)
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



