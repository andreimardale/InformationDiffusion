source('./library_loader.R')
source("./utils.R")
library(stringr)

nb_big_nohash <- readRDS("../Data/correct_model_no_hashtags.rds")

t = initializeData()

train = matrix(ncol = 22)[-c(1),]
colnames(train) = c("comments", "submissions","R_A", "R_B", "R_N", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")
for (p in 1 : (length(t) - 1)) {
  print(paste("Period", p))
  
  t[[p]]$Content = str_replace_all(str_replace_all(t[[p]]$Content, "&gt;.*\n", ""), "\n", "")
  t[[p+1]]$Content = str_replace_all(str_replace_all(t[[p+1]]$Content, "&gt;.*\n", ""), "\n", "")
  T_ = t[[p]]
  
  ### Add a unique ID
  T_$id = ifelse(is.na(T_$Comment.ID), T_$Submission.ID, T_$Comment.ID)
  ###
  
  T_rez1 = getAggregatePredictResults(t[[p]])
  T_rez2 = getAggregatePredictResults(t[[p+1]])
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
  T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
  
  T1_common_authors$prediction = ifelse(T1_common_authors$leave_probability >= 0.9, 1, ifelse(T1_common_authors$leave_probability >= 0.1, 2, 0))
  T2_common_authors$prediction = ifelse(T2_common_authors$leave_probability >= 0.9, 1, ifelse(T2_common_authors$leave_probability >= 0.1, 2, 0))
  
  colnames(T_)[3] = "text"
  
  r = getPredictions(T_)
  T_$leave_prob = r$leave_prob
  T_$prediction = ifelse(T_$leave_prob >= 0.9, 1, ifelse(T_$leave_prob >= 0.1, 2, 0))
  
  for (u in 1 : length(common_authors)) {
    submissions = T_[T_$Author == common_authors[u] & is.na(T_$Comment.ID),]$Submission.ID
    comments =  na.omit(T_[T_$Author == common_authors[u],]$Comment.ID)
    posts = c(submissions, comments)
    
    r_a = 0
    r_b = 0
    r_n = 0
    
    for (i in 1 : length(comments)) {
      parentOfComment = subset(T_, T_$Comment.ID == comments[i], select = c("Parent.ID"))
      parentPrediction = subset(T_, T_$id == parentOfComment$Parent.ID, select = c("prediction"))
      r_a = r_a + sum(parentPrediction$prediction == 0)
      r_b = r_b + sum(parentPrediction$prediction == 1)
      r_n = r_n + sum(parentPrediction$prediction == 2)
    }
    
        
    A = c()
    B = c()
    N = c()
    
    for (i in 1 : length(posts)) {
      post = subset(T_, T_$Parent.ID == posts[i])
      
      A = c(A, sum(post$prediction == 0))
      B = c(B, sum(post$prediction == 1))
      N = c(N, sum(post$prediction == 2))
    }
    
    qA = quantile(A)
    qB = quantile(B)
    qN = quantile(N)
    
    train = rbind(train, c(length(comments), length(submissions), r_a, r_b, r_n, qA, qB, qN, T1_common_authors[u,]$prediction, T2_common_authors[u,]$prediction))
  }
}

train = as.data.frame(train)

save(train, file = "F2_09_extrafeats_improved_data.RData")


load("F2_improved_data.RData")
  
T_agg = train %>%
  group_by(comments, submissions, R_A, R_B, R_N, A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, N1, N2, N3, N4, N5, CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2" & T_agg$labels != "2 2 2"),]

for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL

hist(T_agg_1$NextLabel)
unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(22)))),]

write.csv(unq_train, file = "F2_09_extrafeats_improved_data.csv")

