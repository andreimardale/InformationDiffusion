source('./library_loader.R')
source("./utils.R")

load("../Data/data_circulate/data_dt_brexittweets_sample.Rdata")  ## To be changed with full set
nb_big_nohash <- readRDS("../Data/correct_model_no_hashtags.rds") ## To be changed with the trained version

diffusions = data_dt_brexittweets_sample %>% group_by(clean_text) %>% summarise(count = n())
diffusions = subset(diffusions, diffusions$count > 1)
diffusions$diffusion_id = 1 : dim(diffusions)[1]

data = na.omit(merge(data_dt_brexittweets_sample, diffusions))
colnames(data)[1] = c("Content")
colnames(data)[3] = c("Date")
colnames(data)[5] = c("Author")
data = data[order(data$Date),]
data$Date = as.Date(data$Date)

df = data %>% group_by(diffusion_id) %>% summarise(retweets = sum(retweet == T),
                                              initial_tweets = sum(retweet == F),
                                              unique_authors = n_distinct(Author))


data = merge(data, subset(df, retweets != 0 | initial_tweets == 0 | unique_authors != 1, select = c(1)))
rm(df)

t = splitInTimeframes(data, 3, T)
t = append(t, splitInTimeframes(t[[3]], 3, F))

train = data.frame()

for(p in 1 : (length(t) - 1)){
  print(paste("Period", p))
  
  T_ = t[[p]]
  T_rez1 = getAggregatePredictResultsForTwitter(t[[p]])
  T_rez2 = getAggregatePredictResultsForTwitter(t[[p+1]])
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
  T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
  
  T1_user_activity = T1_common_authors %>%
    group_by(Author) %>%
    summarise(retweets = sum(retweet == T),
              initial_tweets = sum(retweet == F)) %>%
    arrange(Author)
  
  T1_post_success = T1_common_authors[T1_common_authors$retweet == F] %>%
    group_by(Author) %>%
    summarise(initial_tweets = sum(retweet == F),
              dif_sizes = paste0(count, collapse = " "),
              diffusions = paste0(diffusion_id, collapse = " ")) %>%
    arrange(Author)
  T1_post_success$Author = as.character(T1_post_success$Author)
  
  ttt = matrix(ncol = 5)[-c(1),]
  for (i in 1 : dim(T1_post_success)[1]) {
    test = as.numeric(unlist(regmatches(T1_post_success[i,]$dif_sizes, gregexpr("[[:digit:]]+", T1_post_success[i,]$dif_sizes))))
    if(length(test) != 0){
      ttt = rbind(ttt, c(quantile(test)))
    }
    else {
      ttt = rbind(ttt, c(0,0,0,0,0))
    }
  }
  T1_post_success = cbind(T1_post_success$Author, ttt)
  colnames(T1_post_success)[1] = c("Author")
  rm(ttt)
  
  T1_features = merge(T1_user_activity, T1_post_success, by="Author", all.x=TRUE, fill=0);
  colnames(T1_features)[4:8] = c("Q0", "Q25", "Q50", "Q75", "Q100" )
  
  T1_features[,4:8] = as.numeric(as.character(unlist(T1_features[,4:8])))
  T1_features[is.na(T1_features)] = 0
  
  T1_features = unique(merge(T1_features, T_rez1[,c(1, 8)]))
  colnames(T1_features)[9] = c("current_stance")
  
  T1_features = unique(merge(T1_features, T_rez2[,c(1, 8)]))
  colnames(T1_features)[10] = c("next_stance")
  
  
  train = rbind(train, T1_features)
}

train$Author = NULL
colnames(train) = c("replies", "submissions", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")

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


