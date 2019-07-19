source('./library_loader.R')
source("./utils.R")
library(data.table)

nb_big_nohash <- readRDS("./correct_model_no_hashtags.rds")

t_all = initializeData(-1, F)
t_sample = t_all[1:100000,]
tm_ = unique(as.matrix(getTermMatrixWithTM(t_sample, -1, sparsity = 0.9999999999, weightTfIdf)))
mostFrequentWords = as.character(getOverallMostFrequentWords(tm_)$word)
# save(mostFrequentWords, file = "most_frequent_words.RData")
load("~/InformationDiffusion/most_frequent_words.RData")

t = initializeData()

train = data.frame()

for (p in 1 : (length(t) - 1)) {
  
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
  
  tm_ = as.matrix(getTermMatrixWithTM(t, p, sparsity = 0.99999999999999, weightTfIdf))
  tm_ = setDT(as.data.frame(tm_[common_authors,mostFrequentWords[1:100]]), keep.rownames = "Author")[]
  
  T1_features = merge(tm_, T1_common_authors[,c(1, 5)])
  colnames(T1_features)[dim(T1_features)[2]] = c("current_stance")
  
  T1_features = merge(T1_features, T2_common_authors[,c(1, 5)])
  colnames(T1_features)[dim(T1_features)[2]] = c("next_stance")
  
  
  train = rbind(train, T1_features)
}

train$Author = NULL
columns = colnames(train)

colnames(train) = c(paste0("W", 1 : (dim(train)[2] - 2)), "CurrentLabel", "NextLabel")

dim(unique(train))
save(train, file = "F0_data.RData")

#################################################
load("F0_data.RData")

T_agg = train %>%
  group_by(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21, W22, W23, W24, W25, W26, W27, W28, W29, W30, W31, W32, W33, W34, W35, W36, W37, W38, W39, W40, W41, W42, W43, W44, W45, W46, W47, W48, W49, W50, W51, W52, W53, W54, W55, W56, W57, W58, W59, W60, W61, W62, W63, W64, W65, W66, W67, W68, W69, W70, W71, W72, W73, W74, W75, W76, W77, W78, W79, W80, W81, W82, W83, W84, W85, W86, W87, W88, W89, W90, W91, W92, W93, W94, W95, W96, W97, W98, W99, W100, CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2" & T_agg$labels != "2 2 2"),]


for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL

hist(T_agg_1$NextLabel)

unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(102)))),]

write.csv(unq_train, file = "F0_improved_data.csv")

