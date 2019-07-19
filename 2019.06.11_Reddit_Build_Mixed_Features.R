library(dplyr)

#########################
load("F0_improved_data.RData")
train_F0 = train

load("F1_improved_data.RData")
train_F1 = train

load("F2_improved_data.RData")
train_F2 = train

load("F3_improved_data.RData")
train_F3 = train

train_F0$CurrentLabel = NULL
train_F0$NextLabel = NULL

train_F1$CurrentLabel = NULL
train_F1$NextLabel = NULL

train_F2$CurrentLabel = NULL
train_F2$NextLabel = NULL
train_F2$comments = NULL
train_F2$submissions = NULL

colnames(train_F1) = paste0("F1_", 1:7)
colnames(train_F2) = paste0("F2_", 1:18)

train = cbind(train_F0, train_F1, train_F2, train_F3)

T_agg = train %>%
  group_by(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21, W22, W23, W24, W25, W26, W27, W28, W29, W30, W31, W32, W33, W34, W35, W36, W37, W38, W39, W40, W41, W42, W43, W44, W45, W46, W47, W48, W49, W50, W51, W52, W53, W54, W55, W56, W57, W58, W59, W60, W61, W62, W63, W64, W65, W66, W67, W68, W69, W70, W71, W72, W73, W74, W75, W76, W77, W78, W79, W80, W81, W82, W83, W84, W85, W86, W87, W88, W89, W90, W91, W92, W93, W94, W95, W96, W97, W98, W99, W100,
          F1_1,F1_2,F1_3,F1_4,F1_5,F1_6,F1_7,F2_1,
           F2_2,F2_3,F2_4,F2_5,F2_6,F2_7,F2_8,F2_9,
           F2_10,F2_11,F2_12,F2_13,F2_14,F2_15,F2_16,F2_17,
           F2_18,A1,A2,A3,A4,A5,B1,B2,
           B3,B4,B5,N1,N2,N3,N4,N5,
           CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2" & T_agg$labels != "2 2 2"),]
  
for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL



unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(dim(T_agg_1)[2])))),]

write.csv(unq_train, file="F0123_improved_data.csv")

###########################################

toPlot = subset(unq_train, select = c(41,42))
toPlot_grouped = toPlot %>% group_by(CurrentLabel, NextLabel) %>% summarise(count = n())




source('./library_loader.R')
source("./utils.R")

nb_big_nohash = readRDS("../Data/correct_model_no_hashtags.rds")

t = initializeData()

train = matrix(ncol = 17)[-c(1),]
colnames(train) = c("A1", "A2", "A3", "A4", "A5","B1", "B2", "B3", "B4", "B5", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")


for (p in 1:2) {
  print(paste("Period", p))
  t[[p]]$Content = str_replace_all(str_replace_all(t[[p]]$Content, "&gt;.*\n", ""), "\n", "")
  t[[p+1]]$Content = str_replace_all(str_replace_all(t[[p+1]]$Content, "&gt;.*\n", ""), "\n", "")
  
  T_ = t[[p]]
  colnames(T_)[3] = "text"
  
  T_rez1 = getAggregatePredictResults(t[[p]])
  T_rez2 = getAggregatePredictResults(t[[p+1]])
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
  T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
  
  T1_common_authors$prediction = ifelse(T1_common_authors$leave_probability >= 0.75, 1, ifelse(T1_common_authors$leave_probability >= 0.25, 2, 0))
  T2_common_authors$prediction = ifelse(T2_common_authors$leave_probability >= 0.75, 1, ifelse(T2_common_authors$leave_probability >= 0.25, 2, 0))
  
  r = getPredictions(T_)
  T_$leave_prob = r$leave_prob
  T_$prediction = ifelse(T_$leave_prob >= 0.75, 1, ifelse(T_$leave_prob >= 0.25, 2, 0))
  
  for (u in 1 : length(common_authors)) {
    submissions = unique(T_[T_$Author == common_authors[u],]$Submission.ID)
    A = c()
    B = c()
    N = c()
    
    for (i in 1 : length(submissions)) {
      submission = subset(T_, T_$Submission.ID == submissions[i])
      
      n = dim(submission)[1]
      pA = sum(submission$prediction == 0) / n
      pB = sum(submission$prediction == 1) / n
      pN = sum(submission$prediction == 2) / n
      
      A = c(A,pA)
      B = c(B,pB)
      N = c(N,pN)
    }
    qA = quantile(A)
    qB = quantile(B)
    qN = quantile(N)
    train = rbind(train, c(qA, qB, qN, T1_common_authors[u,]$prediction, T2_common_authors[u,]$prediction))
  }
}
train = as.data.frame(train)

T_ = data.frame()
for (i in 1:14) {
  T_ = rbind(T_, t[[i]])
}
T_$Content = str_replace_all(str_replace_all(T_$Content, "&gt;.*\n", ""), "\n", "")
colnames(T_)[3] = "text"
r = getPredictions(T_)
T_$leave_prob = r$leave_prob

hist(T_$leave_prob, breaks = 100)

T_leavers = subset(T_, T_$leave_prob < 0.05)

utl = textTinyR::sparse_term_matrix$new(vector_data = T_leavers$text, file_data = NULL, document_term_matrix = TRUE)

tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = T,
                     remove_numbers = T, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                     threads = 6, verbose = T)

plotWordCloud(getOverallMostFrequentWords(tm), "timeframe_all.png")













T_agg = train %>%
  group_by(A1, A2, A3, A4, A5,B1, B2, B3, B4, B5, N1, N2, N3, N4, N5, CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2" & T_agg$labels != "2 2 2"),]

for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL

hist(T_agg_1$NextLabel)


  