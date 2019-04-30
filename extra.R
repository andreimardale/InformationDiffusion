library(gtools)
library(ggplot2)
library(gplots)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library(topicmodels)
library(tidyr)
library(tidytext)
library(dplyr)

setwd("../Desktop/M1 Internship/Code")
source("./utils.R")

# Reading the Submissions Data
diffusions_submissions = readSubmissions()

# Reading the Comments Data
diffusions_comments = readComments()

# Merging the submissions and the comments
posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)

wd = c(as.Date("2015-11-16", format="%Y-%m-%d"), as.Date("2016-06-25", format="%Y-%m-%d"),
       as.Date("2016-07-14", format="%Y-%m-%d"), as.Date("2016-07-28", format="%Y-%m-%d"),
       as.Date("2016-12-08", format="%Y-%m-%d"), as.Date("2017-01-27", format="%Y-%m-%d"),
       as.Date("2017-03-30", format="%Y-%m-%d"), as.Date("2017-06-20", format="%Y-%m-%d"),
       as.Date("2018-07-09", format="%Y-%m-%d"), as.Date("2018-09-22", format="%Y-%m-%d"),
       as.Date("2018-11-16", format="%Y-%m-%d"), as.Date("2018-11-26", format="%Y-%m-%d"),
       as.Date("2019-01-16", format="%Y-%m-%d"), as.Date("2019-03-15", format="%Y-%m-%d"),
       as.Date("2019-03-22", format="%Y-%m-%d"), as.Date("2019-03-30", format="%Y-%m-%d"),
       as.Date("2019-04-06", format="%Y-%m-%d"))

rez = cut.Date(posts$Date, breaks=wd, labels = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", 
                                                 "T8", "T9", "T10", "T11", "T12", "T13", "T14",
                                                 "T15", "T16"))
summary(rez)
t = split(posts, rez)

############# Test ################
T1.content = paste(unlist(t[[1]]$Content), collapse =" ")
T2.content = paste(unlist(t[[2]]$Content), collapse =" ")
T3.content = paste(unlist(t[[3]]$Content), collapse =" ")
T4.content = paste(unlist(t[[4]]$Content), collapse =" ")
T5.content = paste(unlist(t[[5]]$Content), collapse =" ")
T6.content = paste(unlist(t[[6]]$Content), collapse =" ")
T7.content = paste(unlist(t[[7]]$Content), collapse =" ")
T8.content = paste(unlist(t[[8]]$Content), collapse =" ")
T9.content = paste(unlist(t[[9]]$Content), collapse =" ")
T10.content = paste(unlist(t[[10]]$Content), collapse =" ")
T11.content = paste(unlist(t[[11]]$Content), collapse =" ")
T12.content = paste(unlist(t[[12]]$Content), collapse =" ")
T13.content = paste(unlist(t[[13]]$Content), collapse =" ")
T14.content = paste(unlist(t[[14]]$Content), collapse =" ")
T15.content = paste(unlist(t[[15]]$Content), collapse =" ")
T16.content = paste(unlist(t[[16]]$Content), collapse =" ")


content = c(T1.content, T2.content, T3.content, T4.content, T5.content, T6.content, T7.content,
            T8.content, T9.content, T10.content, T11.content, T12.content, T13.content, T14.content,
            T15.content, T16.content)

utl = textTinyR::sparse_term_matrix$new(vector_data = content, file_data = NULL, document_term_matrix = TRUE)

tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = T,
                     remove_numbers = T, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)

d1 = getMostFrequentWords(tm, 1)
d2 = getMostFrequentWords(tm, 2)
d3 = getMostFrequentWords(tm, 3)
d4 = getMostFrequentWords(tm, 4)
d5 = getMostFrequentWords(tm, 5)
d6 = getMostFrequentWords(tm, 6)
d7 = getMostFrequentWords(tm, 7)
d8 = getMostFrequentWords(tm, 8)
d9 = getMostFrequentWords(tm, 9)
d10 = getMostFrequentWords(tm, 10)
d11 = getMostFrequentWords(tm, 11)
d12 = getMostFrequentWords(tm, 12)
d13 = getMostFrequentWords(tm, 13)
d14 = getMostFrequentWords(tm, 14)
d15 = getMostFrequentWords(tm, 15)
d16 = getMostFrequentWords(tm, 16)

plotWordCloud(d1, "timeframe_1.png")
plotWordCloud(d2, "timeframe_2.png")
plotWordCloud(d3, "timeframe_3.png")
plotWordCloud(d4, "timeframe_4.png")
plotWordCloud(d5, "timeframe_5.png")
plotWordCloud(d6, "timeframe_6.png")
plotWordCloud(d7, "timeframe_7.png")

plotWordCloud(d8, "timeframe_8.png")
plotWordCloud(d14, "timeframe_14.png")

d_overall = getOverallMostFrequentWords(tm)
plotWordCloud(d_overall, "timeframe_all.png")


#Removing sparse terms
m_adj <- as.matrix(utl$Term_Matrix_Adjust(sparsity_thresh = 0.1))
C = getCosineSimilarity(m_adj)

#Plotting the heatmaps with the evolution of thematics
png("heatmaps.png", width = 1600, height = 1200, res = 300, pointsize = 8)
heatmap.2(C, main = "Cos. similarity between the 16 periods", dendrogram = "none", Rowv = FALSE, Colv = FALSE, density.info="none", scale = "none", trace ="none") 
dev.off()
