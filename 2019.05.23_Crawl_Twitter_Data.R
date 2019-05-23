source('./library_loader.R')
source("./utils.R")
library(readr)
library(stringr)


remain_tweets <- read_csv("../Data/tweets_remain.csv")
remain_tweets = remain_tweets[!duplicated(remain_tweets$text),]


leave_tweets <- read_csv("../Data/tweets_leave.csv")
leave_tweets = leave_tweets[!duplicated(leave_tweets$text),]


T_leave = leave_tweets %>%
  group_by(user) %>%
  summarise(nr_of_posts = n(), Content = paste0(text, collapse = " ")) %>%
  arrange(desc(nr_of_posts))


tweets = rbind(remain_tweets, leave_tweets)
tweets = tweets[tweets$user != "Uppington",]
tweets = tweets[tweets$user != "wikileaksbot",]
tweets = tweets[tweets$user != "consolting",]
tweets = tweets[tweets$user != "InforLatam",]
tweets = tweets[tweets$user != "infor_brasil",]
tweets = tweets[tweets$user != "SofonDE",]
tweets = tweets[tweets$user != "thevobot",]
tweets = tweets[tweets$user != "mosaicbottester",]
tweets = tweets[tweets$user != "robots_",]
tweets = tweets[tweets$user != "Otherbots",]
tweets = tweets[tweets$user != "bungkercorp",]
tweets = tweets[tweets$user != "technews84",]
tweets = tweets[tweets$user != "gabbycorsalas",]

T_ = tweets %>%
  group_by(user) %>%
  summarise(nr_of_posts = n(), Content = paste0(text, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

View(T_[,c(1,2) ])


leave_hashtags = c("#voteleave", "#inorout", "#voteout", "#borisjohnson", "#projecthope", "#independenceday", "#ivotedleave", "#boris",
"#lexit", "#takebackcontrol", "#labourleave", "#no2eu", "#betteroffout", "#june23")

text = "Happy #fourth #independenceday	"
hashes = unlist(str_extract_all(text, "#\\S+"))

leavs = 0
remains = 0
for(i in hashes) {
  if (i %in% leave_hashtags)
    leavs = leavs + 1
}

