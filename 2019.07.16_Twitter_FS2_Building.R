source('./library_loader.R')
source("./utils.R")
library('multidplyr')

no_cores = 18

load("../Data/data_dt_brexittweets.Rdata")  ## To be changed with full set
nb_big_nohash <- readRDS("./nb_with_hashtags_july.rds") ## To be changed with the trained version

diffusions = data_dt_brexittweets %>% group_by(clean_text) %>% summarise(count = n())
diffusions = subset(diffusions, diffusions$count > 1)
diffusions$diffusion_id = 1 : dim(diffusions)[1]

data = na.omit(merge(data_dt_brexittweets, diffusions))
colnames(data)[1] = c("Content")
colnames(data)[3] = c("Date")
colnames(data)[5] = c("Author")
data = data[order(data$Date),]
data$Date = as.Date(data$Date)

cluster <- new_cluster(no_cores)
df = data %>%
  group_by(diffusion_id) %>%
  partition(cluster) %>%
  summarize(retweets = sum(retweet == T),
            initial_tweets = sum(retweet == F),
            unique_authors = n_distinct(Author)) %>%
  collect() %>%
  arrange(diffusion_id)

data = merge(data, subset(df, retweets != 0 | initial_tweets == 0 | unique_authors != 1, select = c(1)))

t = splitInTimeframes(data, 3, T)
t = append(t, splitInTimeframes(t[[3]], 3, F))

train = data.frame()

for(p in 1 : (length(t) - 1)){
  print(paste("Period", p))
  
  T_rez1 = getAggregatePredictResultsForTwitter(t[[p]])
  T_rez2 = getAggregatePredictResultsForTwitter(t[[p+1]])
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
  T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
  
  T1_user_activity = T1_common_authors %>%
    group_by(Author) %>%
    partition(cluster) %>%
    summarize(retweets = sum(retweet == T),
              initial_tweets = sum(retweet == F)) %>%
    collect() %>%
    arrange(Author)
  
  T1_post_success = T1_common_authors[T1_common_authors$retweet == F] %>%
    group_by(Author) %>%
    partition(cluster) %>%
    summarise(initial_tweets = sum(retweet == F),
              dif_sizes = paste0(count, collapse = " "),
              diffusions = paste0(diffusion_id, collapse = " ")) %>%
    collect() %>%
    arrange(Author)
  
  T1_post_success$Author = as.character(T1_post_success$Author)
  
  # Going parallel #
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)  
  
  ttt = foreach(author = 1 : dim(T1_post_success)[1], .combine = rbind)  %dopar%  
    {
      diffs = as.numeric(unlist(regmatches(T1_post_success[author,]$diffusions, gregexpr("[[:digit:]]+", T1_post_success[author,]$diffusions))))
      
      A = c()
      B = c()
      N = c()
      
      for (dif in diffs) {
        difusin = subset(T_rez1, diffusion_id == dif)
        
        A = c(A, sum(difusin$prediction == 0))
        B = c(B, sum(difusin$prediction == 1))
        N = c(N, sum(difusin$prediction == 2))
      }
      
      qA = quantile(A)
      qB = quantile(B)
      qN = quantile(N)
      
      return(c(qA, qB, qN))
    }
  
  stopCluster(cl)
  ####
  
  
  T1_post_success = cbind(T1_post_success$Author, ttt)
  colnames(T1_post_success)[1] = c("Author")
  rm(ttt)
  
  T1_features = merge(T1_user_activity, T1_post_success, by="Author", all.x=TRUE, fill=0);
  colnames(T1_features)[4:18] = c("A0", "A25", "A50", "A75", "A100",
                                  "B0", "B25", "B50", "B75", "B100",
                                  "N0", "N25", "N50", "N75", "N100")
  
  #################
  
  T1_post_retweets = T1_common_authors[T1_common_authors$retweet == T] %>%
    group_by(Author) %>%
    partition(cluster) %>%
    summarize(initial_tweets = sum(retweet == T),
              dif_sizes = paste0(count, collapse = " "),
              diffusions = paste0(diffusion_id, collapse = " ")) %>%
    collect() %>%
    arrange(Author)
  
  
  T1_post_retweets$Author = as.character(T1_post_retweets$Author)
  
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)  
  
  ttt = foreach(author = 1 : dim(T1_post_retweets)[1], .combine = rbind)  %dopar%  
    {
      diffs = as.numeric(unlist(regmatches(T1_post_retweets[author,]$diffusions, gregexpr("[[:digit:]]+", T1_post_retweets[author,]$diffusions))))
      
      r_a = 0
      r_b = 0
      r_n = 0
      
      for (dif in diffs) {
        difusin = subset(T_rez1, diffusion_id == dif && retweet == FALSE)
        difusin = difusin[order(difusin$Date),]
        
        parentPrediction = difusin[1,]$prediction
        if(is.na(parentPrediction))
          next
        
        r_a = r_a + sum(parentPrediction$prediction == 0)
        r_b = r_b + sum(parentPrediction$prediction == 1)
        r_n = r_n + sum(parentPrediction$prediction == 2)
        
      }
      
      return(c(r_a, r_b, r_n))
    }
  
  stopCluster(cl)
  
  T1_post_retweets = cbind(T1_post_retweets$Author, ttt)
  colnames(T1_post_retweets)[1:4] = c("Author", "r_a", "r_b", "r_n" )
  rm(ttt)
  
  T1_features = merge(T1_features, T1_post_retweets, by="Author", all.x=TRUE, fill=0);
  
  
  T1_features[,4:21] = as.numeric(as.character(unlist(T1_features[,4:21])))
  T1_features[is.na(T1_features)] = 0
  
  T1_features = unique(merge(T1_features, T_rez1[,c(1, 8)]))
  colnames(T1_features)[22] = c("current_stance")
  
  T1_features = unique(merge(T1_features, T_rez2[,c(1, 8)]))
  colnames(T1_features)[23] = c("next_stance")
  
  
  #######################
  
  
  train = rbind(train, T1_features)
}

train$Author = NULL

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


