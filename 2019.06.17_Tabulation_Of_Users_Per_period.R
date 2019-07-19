source('./library_loader.R')
source("./utils.R")

nb_big_nohash = readRDS("../Data/correct_model_no_hashtags.rds")

t = initializeData()

periods = c()

for (p in 1:14) {
  print(paste("Period", p))
  
  T_rez1 = t[[p]] %>%
    group_by(Author) %>%
    summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(Author)
  
  T_rez2 = t[[p+1]] %>%
    group_by(Author) %>%
    summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(Author)
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  for (u in 1 : length(common_authors)) {
    periods = rbind(periods, c(p))
  }
}

load("F1_improved_data.RData")
train_F1 = train

load("F2_improved_data.RData")
train_F2 = train

load("F3_improved_data.RData")
train_F3 = train

train_F1$CurrentLabel = NULL
train_F1$NextLabel = NULL

train_F2$CurrentLabel = NULL
train_F2$NextLabel = NULL
train_F2$comments = NULL
train_F2$submissions = NULL

colnames(train_F1) = paste0("F1_", 1:7)
colnames(train_F2) = paste0("F2_", 1:18)

train_data = cbind(periods, train_F1, train_F2, train_F3)

for (i in 1:14) {
  getDataPerPeriod(train_data, i)
}

T_agg = train_data %>%
  group_by(F1_1,F1_2,F1_3,F1_4,F1_5,F1_6,F1_7,F2_1,
           F2_2,F2_3,F2_4,F2_5,F2_6,F2_7,F2_8,F2_9,
           F2_10,F2_11,F2_12,F2_13,F2_14,F2_15,F2_16,F2_17,
           F2_18,A1,A2,A3,A4,A5,B1,B2,
           B3,B4,B5,N1,N2,N3,N4,N5,
           CurrentLabel) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "), perds = paste0(periods, collapse = " "))

T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2"),]

for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
  
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$perds, gregexpr("[[:digit:]]+", T_agg_1[i,]$perds))))
  T_agg_1[i,"Period"] =  as.numeric(names(which.max(table(test))))
}

T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL
T_agg_1$perds = NULL

data = subset(T_agg_1, select = c(43, 42))

#### For predictions
predictions <- read.csv("~/Desktop/M1 Internship/Code/data/predictions.csv")
pred_rez = c()
pred_t = c(31, 24, 13, 9, 19, 24, 78, 54, 60, 41, 124, 158, 94, 95)
for (i in 1:dim(predictions)[1]) {
  pred_rez[i] = as.numeric(predictions[i,3]) / as.numeric(pred_t[as.numeric(predictions[i,1])])
}
predictions$norm_count = pred_rez



####


rez = c()
t = data %>% group_by(Period) %>% summarise(count = n())
data_grouped = data %>% group_by(Period, NextLabel) %>% summarise(count = n())
for (i in 1:dim(data_grouped)[1]) {
  rez[i] = as.numeric(data_grouped[i,3]) / as.numeric(t[as.numeric(data_grouped[i,1]), 2])
}

data_grouped$norm_count = rez
data_grouped = subset(data_grouped, data_grouped$NextLabel != 2)

data_grouped$NextLabel = factor(data_grouped$NextLabel, labels = c("Against", "Brexit"))

### To remove if we don't want predictions ####
data_grouped <- read.csv("~/Desktop/M1 Internship/Code/data_grouped.csv")
data_grouped = data_grouped[order(data_grouped$Period),]
###

png("user_transitions_and_predictions.png", width = 3200, height = 1800, res = 300)
ggplot(data_grouped, aes(Period, norm_count, fill = NextLabel, width = width)) + 
  geom_bar(position = 'dodge2', stat = 'identity') +
  geom_rect(aes(xmin = 2, xmax = 4, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.003)+
  scale_x_continuous(name = "Transition", breaks = c(1:14), labels = paste0("T", 1:14, "-T", 2:15)) +
  scale_y_continuous(name = "Percentage of Users") +
  ggtitle("Ratio of neutral user transitions per timeframe") +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold"), #family = "Tahoma", -- not supported by PDF
        text = element_text(size = 18), #family = "Tahoma", -- not supported by PDF
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.position = "bottom") +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = c("#8BB6FF","#F36767","#2C2CDB","#DB2C2C"))+
  labs(fill = "Next Stance:")
dev.off()



df2 <- data_no_2 %>%
  group_by(NextLabel, Period) %>%
  summarise(count = n())
df2$NextLabel = as.factor(df2$NextLabel)

ggplot(df2, aes(Period, count, fill = NextLabel)) +
  geom_bar(stat = 'identity')

data_no_2$NextLabell = factor(data_no_2$NextLabel, labels = c("Against", "Brexit"))
png("user_transitions.png", width = 3200, height = 1800, res = 300)
ggplot(data_no_2, aes(Period)) +
  geom_bar(position = 'dodge', aes(fill = NextLabell)) +
  scale_x_continuous(name = "Period", breaks = c(1:14), labels = paste0("T", 1:14)) +
  scale_y_continuous(name = "Users") +
  ggtitle("Number of neutral user transitions per timeframe") +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold"), #family = "Tahoma", -- not supported by PDF
        text = element_text(size = 18), #family = "Tahoma", -- not supported by PDF
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 16),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Paired")+
  labs(fill = "Next Stance:")
dev.off()











getDataPerPeriod <- function(train_data, period = 12) {
  train_data1 = subset(train_data, train_data$periods == period)
  train_data1$periods = NULL
  
  T_agg = train_data1 %>%
    group_by(F1_1,F1_2,F1_3,F1_4,F1_5,F1_6,F1_7,F2_1,
             F2_2,F2_3,F2_4,F2_5,F2_6,F2_7,F2_8,F2_9,
             F2_10,F2_11,F2_12,F2_13,F2_14,F2_15,F2_16,F2_17,
             F2_18,A1,A2,A3,A4,A5,B1,B2,
             B3,B4,B5,N1,N2,N3,N4,N5,
             CurrentLabel) %>%
    summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))
  
  T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2"),]
  
  for(i in 1 : dim(T_agg_1)[1]){
    test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
    T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
  }
  
  T_agg_1$nr_of_posts = NULL
  T_agg_1$labels = NULL
  T_agg_1$perds = NULL
  
  T_agg_1 = (subset(T_agg_1, T_agg_1$CurrentLabel == 2))
  unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(42)))),]
  
  write.csv(unq_train, file=paste0("data/F4_T", period, "_data.csv"))
}
