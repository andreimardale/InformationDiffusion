source('./library_loader.R')
source("./utils.R")

nb_big_nohash <- readRDS("../Data/correct_model_no_hashtags.rds")

t = initializeData()

train = matrix(ncol = 9)[-c(1),]

for(p in 1 : (length(t) - 1)){
  print(paste("Period", p))
  
  t[[p]]$Content = str_replace_all(str_replace_all(t[[p]]$Content, "&gt;.*\n", ""), "\n", "")
  t[[p+1]]$Content = str_replace_all(str_replace_all(t[[p+1]]$Content, "&gt;.*\n", ""), "\n", "")
  
  T_ = t[[p]]
  T_rez1 = getAggregatePredictResults(t[[p]])
  T_rez2 = getAggregatePredictResults(t[[p+1]])
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
  T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
  
  T1_common_authors$prediction = ifelse(T1_common_authors$leave_probability >= 0.75, 1, ifelse(T1_common_authors$leave_probability >= 0.25, 2, 0))
  T2_common_authors$prediction = ifelse(T2_common_authors$leave_probability >= 0.75, 1, ifelse(T2_common_authors$leave_probability >= 0.25, 2, 0))


  t1= T_[T_$Author %in% common_authors,]
  T_agg = t1 %>%
    group_by(Author) %>%
    summarise(comments = sum(!is.na(Comment.ID)),
              submissions = sum(is.na(Comment.ID)),
              replies_per_subm = paste0(NofComments, collapse = " ")) %>%
    arrange(Author)
  
  ttt = matrix(ncol = 5)[-c(1),]
  for (i in 1 : dim(T_agg)[1]) {
    test = as.numeric(unlist(regmatches(T_agg[i,]$replies_per_subm, gregexpr("[[:digit:]]+", T_agg[i,]$replies_per_subm))))
    if(length(test) != 0){
      ttt = rbind(ttt, c(quantile(test)))
    }
    else {
      ttt = rbind(ttt, c(0,0,0,0,0))
    }
  }
  
  T_agg$replies_per_subm = NULL
  T_agg = cbind(T_agg, ttt)
  T_agg$CurrentLabel = T1_common_authors$prediction
  T_agg$NextLabel = T2_common_authors$prediction
  
  train = rbind(train, T_agg)
}

train$Author = NULL
colnames(train) = c("replies", "submissions", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")
train = as.data.frame(train)

save(train, file = "F1_improved_data.RData")
#####################################

load("F1_improved_data.RData")

T_agg = train %>%
  group_by(replies, submissions, N1, N2, N3, N4, N5, CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2"& T_agg$labels != "2 2 2"),]

for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL

hist(T_agg_1$NextLabel)
unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(9)))),]

write.csv(unq_train, file = "F1_improved_data.csv")


