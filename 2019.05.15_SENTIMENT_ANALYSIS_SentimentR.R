source('./library_loader.R')
source("./utils.R")
library(sentimentr)

# Initialize data
t = initializeData()
PERIOD = 3

t_3 = t[[3]]
t_3$Content = apply(as.data.frame(t_3$Content), 1, function(x) gsub("&gt;(.*?)\n\n", "", x))

sentiment = sentiment_by(t_3$Content)

t_3$Score = NULL
t_3$NofComments = NULL
t_3$ave_sentiment = sentiment$ave_sentiment
t_3$sd = sentiment$sd

T_3 = t_3 %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

sentiment = sentiment_by(T_3$text)
T_3$ave_sentiment = sentiment$ave_sentiment
T_3$sd = sentiment$sd
 