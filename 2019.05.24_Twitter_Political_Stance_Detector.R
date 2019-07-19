source('./library_loader.R')
source("./utils.R")
library("stringr")
library(quanteda)
library(caret)

load("../Data/data_dt_brexittweets.Rdata")

# remove retweets
data_dt_brexittweets = data_dt_brexittweets[!duplicated(data_dt_brexittweets[,c('clean_text')]),]

MIN_THRESHOLD_POSTS = 50

LEAVE_HASHTAGS = c("#voteleave", "#inorout", "#voteout", "#takecontrol", "#borisjohnson", "#projecthope", "#independenceday",
                   "#ivotedleave", "#projectfear", "#britain", "#boris", "#lexit", "#go", "#takebackcontrol", "#labourleave",
                   "#no2eu", "#betteroffout", "#june23", "#democracy")

REMAIN_HASHTAGS = c("#strongerin", "#intogether", "#infor", "#votein", "#libdems", "#voting", "#incrowd", "#bremain", "#greenerin")


T_ = data_dt_brexittweets %>%
  group_by(user_screen_name) %>%
  summarise(nr_of_posts = n(), text = paste0(clean_text, collapse = " ")) %>%
  arrange(desc(nr_of_posts))


T_$important_hashes = apply(as.data.frame(T_$text), 1, function(text) {
  hashes = unlist(str_extract_all(text, "#\\S+"))
  
  leavs = 0
  remains = 0
  for(i in hashes) {
    if (tolower(i) %in% LEAVE_HASHTAGS)
      leavs = leavs + 1
    if (tolower(i) %in% REMAIN_HASHTAGS)
      remains = remains + 1
  }
  return(leavs+remains)
})

T_ = T_[T_$important_hashes > 0,]

T_ = T_[T_$nr_of_posts >= MIN_THRESHOLD_POSTS,]

T_$leave_score = apply(as.data.frame(T_$text), 1, function(text) {
  hashes = unlist(str_extract_all(text, "#\\S+"))
  
  leavs = 0
  remains = 0
  for(i in hashes) {
    if (tolower(i) %in% LEAVE_HASHTAGS)
      leavs = leavs + 1
    if (tolower(i) %in% REMAIN_HASHTAGS)
      remains = remains + 1
  }
  return(leavs-remains)
})

T_ = T_[order(T_$leave_score, decreasing = T),]
T_$text = tolower(T_$text)

T_leave = T_[1:round(0.1 * dim(T_)[1]),]
T_leave$sent = 1

T_remain = T_[(dim(T_)[1] - round(0.1 * dim(T_)[1])) : dim(T_)[1] ,]
T_remain$sent = 0

T_Bound = rbind(T_leave, T_remain)

corp_twitter <- corpus(T_Bound)

dfmat_hashtags = dfm(corp_twitter, select = c("#*", '@*'), remove_twitter = FALSE) %>% 
  dfm_remove(c(stopwords('en'), LEAVE_HASHTAGS, REMAIN_HASHTAGS)) %>%
  dfm_trim(min_termfreq = 5)

dfmat_tweets <- dfm(corp_twitter, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE,
                    remove = c('*.tt', '*.uk', '*.com', 'rt')) %>%
  dfm_remove(pattern = ("@*"))  %>% 
  dfm_remove(pattern = ("#*"))  %>% 
  dfm_remove(c(stopwords('en'))) %>% 
  dfm_remove("\\p{Z}", valuetype = "regex") %>%
  dfm_wordstem(language = "english") %>%
  dfm_select(min_nchar = 3) %>%
  dfm_trim(min_termfreq = 5)


View(featnames(dfmat_tweets))

# dfmat = dfmat_tweets
dfmat = cbind(dfmat_hashtags, dfmat_tweets)

ndoc(dfmat)
nfeat(dfmat)

set.seed(300)
id_train <- sample(1:ndoc(dfmat), round(0.8 * ndoc(dfmat)), replace = FALSE)

# create docvar with ID
docvars(dfmat, "id_numeric") <- 1:ndoc(dfmat)
docvars(dfmat, "Sentiment") <- T_Bound$sent

# get training set
dfmat_training <- dfm_subset(dfmat, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(dfmat, !id_numeric %in% id_train)

tmod_nb <- textmodel_nb(dfmat_training, docvars(dfmat_training, "Sentiment"))
summary(tmod_nb)

#### Training Set #####
runClassifier(dfmat_training, tmod_nb)

#### Test Set #####
runClassifier(dfm_match(dfmat_test, features = featnames(dfmat_training)), tmod_nb)

super_model <- readRDS("./final_model.rds")
saveRDS(tmod_nb, "nb_with_hashtags_july.rds")

######### Utils ############
runClassifier <- function(dfmat, tmod_nb) {
  actual_class <- docvars(dfmat, "Sentiment")
  predicted_class <- predict(tmod_nb, newdata = dfmat)
  tab_class <- table(actual_class, predicted_class)
  
  print(confusionMatrix(tab_class, mode = "everything"))
}

splitInTimeframes <- function(posts, N, equal = F) {
  if (equal) {
    pts_N_cut = N
    labels_N_cut = paste0("T", 1:N)
  }
  
  tmp = cut(posts$created_at, 15)
  levels(tmp)
  
  dates = cut(posts$created_at, breaks=pts_N_cut, labels = labels_N_cut)
  print(summary(dates))
  t = split(posts, dates)
  return(t)
}
