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

# Number of diffusions ####
print(dim(diffusions)[1])

# Number of retweets #####
print(dim(data)[1] - dim(diffusions)[1])

# Number of unique users #
length(unique(data$Author))

# Number of tweets per user #
authors = data %>% group_by(Author) %>% summarise(count = n())

no_of_messages_per_author_ecdf <- ecdf( authors$count )
df_log <- data.frame( x = log(sort(authors$count)), y = log(1-no_of_messages_per_author_ecdf(sort(authors$count) )))

png("ccdf_tweets_per_author_prob.png", width = 3200, height = 1800, res = 300)
ggplot(data=df_log, aes(x, y) ) +
  geom_line() +
  geom_point(color="blue")+ labs(x = "log(Number of Tweets)") + labs(y = "log(Probability)")
dev.off()

# Number of retweets per diffusion #
no_of_messages_per_diffusion_ecdf <- ecdf( diffusions$count )
df_log <- data.frame( x = log(sort(diffusions$count)), y = log(1-no_of_messages_per_diffusion_ecdf(sort(diffusions$count) )))

png("ccdf_tweets_per_diffusion_prob.png", width = 3200, height = 1800, res = 300)
ggplot(data=df_log, aes(x, y) ) +
  geom_line() +
  geom_point(color="blue")+ labs(x = "log(Number of Tweets)") + labs(y = "log(Probability)")
dev.off()

quantile(diffusions$count)

# Density of Tweets in time #
png("densOfTweets.png", width = 3200, height = 1800, res = 300)
ggplot(data, aes(as.Date(data$Date))) +
  geom_density(bw = 5) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10))
dev.off()

###################################
### Splitting in 3 time-periods ###
###################################

data$Date = as.Date(data$Date)
t = splitInTimeframes(data, 3, T)

for (p in 1:length(t)) {
  t_p = t[[p]]
  
  print(t_p$Date[1]) #Starting date
  print(t_p$Date[dim(t_p)[1]]) #Ending date
  
  d_p = t_p %>% group_by(diffusion_id) %>% summarise(count = n())
  d_p_cross_periods = subset(d_p, d_p$count == 1)
  
  cross_periods = merge(data, d_p_cross_periods, by = 'diffusion_id')
  
  cross_periods$count.y = cross_periods$diffusion_id
  cross_periods$diffusion_id = NULL
  colnames(cross_periods)[6] = c("count")
  colnames(cross_periods)[7] = c("diffusion_id")
  
  t_p = rbind(t_p, cross_periods)
  
  print(length(unique(t_p$Author)))   # Number of unique users
  print(length(unique(t_p$diffusion_id))) # Number of diffusion starter tweets
  print(dim(t_p)[1] - length(unique(t_p$diffusion_id))) # Number of diffusion retweets
 
  t[[p]] = t_p
}

rm(t_p)
rm(d_p)
rm(d_p_cross_periods)
rm(cross_periods)
rm(df_log)

for (p in 1 : length(t)) {
  T_agg = t[[p]] %>%
    group_by(Author) %>%
    summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(Author)
  
  r = getPredictions(T_agg)
  
  T_agg = data.frame(T_agg$Author, r$leave_prob)
  colnames(T_agg) = c("Author", "leave_prob")

  T_agg = merge(t[[p]], T_agg, by = "Author")
  T_agg$current_stance = ifelse(T_agg$leave_prob >= 0.75, 1, ifelse(T_agg$leave_prob >= 0.25, 2, 0))
  T_agg$leave_prob = NULL
  
  
  T_agg_by_diffusion = T_agg %>%
    group_by(diffusion_id) %>%
    summarise(diffusions = paste0(current_stance, collapse = " "))
  
  entropies = apply(T_agg_by_diffusion, 1, function(x) {
    diffs = as.numeric(unlist(regmatches(x[2], gregexpr("[[:digit:]]+", x[2]))))
    
    zero = sum(diffs == 0) / length(diffs)
    one = sum(diffs == 1) / length(diffs)
    two = sum(diffs == 2) / length(diffs)
    
    return(c(zero, one, two))
  })
  
  entropies = as.data.frame(t(entropies))
  colnames(entropies) = c("Against", "Brexit", "Neutral")
  
  print(paste("Diffusions with just one stance", sum(entropies == 1), "out of", dim(entropies)[1] ))
  
  png(paste0("T_", p ,"_stances_probabilities.png"), width = 3200, height = 1600, res = 300)
  plot(ggplot(entropies, aes(x=Against, y=Brexit)) + geom_point(col = adjustcolor("steelblue", alpha=0.07)))
  dev.off()
  
}






