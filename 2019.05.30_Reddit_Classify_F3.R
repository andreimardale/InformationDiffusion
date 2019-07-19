source('./library_loader.R')
source("./utils.R")

nb_big_nohash = readRDS("../Data/correct_model_no_hashtags.rds")

t = initializeData()

train = matrix(ncol = 17)[-c(1),]
colnames(train) = c("A1", "A2", "A3", "A4", "A5","B1", "B2", "B3", "B4", "B5", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")

for (p in 1:14) {
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
    View(T_[T_$Author == common_authors[u],])
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
      
      if(pA != 0 && pB != 0) {
        print(submission$Submission.ID)
      }
      
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
save(train, file = "F3_improved_data.RData")

#################################################
load("F3_improved_data.RData")

T_agg = train %>%
  group_by(A1, A2, A3, A4, A5,B1, B2, B3, B4, B5, N1, N2, N3, N4, N5, CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2" & T_agg$labels != "2 2 2"),]

T_agg_1 = T_agg_1[T_agg_1$CurrentLabel != 1 | (T_agg_1$labels != "2"),]

T_agg_1 = T_agg_1[T_agg_1$CurrentLabel != 0 | (T_agg_1$labels != "2"),]


for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL

hist(T_agg_1$NextLabel)

unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(17)))),]

write.csv(unq_train, file = "F3_improved_data.csv")
  