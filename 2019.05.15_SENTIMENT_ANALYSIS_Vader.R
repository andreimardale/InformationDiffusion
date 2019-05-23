source('./library_loader.R')
source("./utils.R")

# Initialize reticulate and set it to use python3
library('reticulate')
use_python("/usr/bin/python3", required=TRUE)

# Initialize data
t = initializeData()
PERIOD = 4

T_ = t[[PERIOD]] %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

# Importing reticulate style the VADER module
vaderSentiment = import("vaderSentiment.vaderSentiment")
analyser = vaderSentiment$SentimentIntensityAnalyzer()

t_3 = t[[PERIOD]]
t_3$Content = apply(as.data.frame(t_3$Content), 1, function(x) gsub("&gt;(.*?)\n\n", "", x))

r = apply(as.data.frame(t_3$Content), 1, function(x) analyser$polarity_scores(x))
sentiment_t3 = cbind(t_3, data.frame(matrix(unlist(r), nrow=length(r), byrow=T, dimnames = list(NULL, c("neg", "neu", "pos", "compound")))))
sentiment_t3$NofComments = NULL
sentiment_t3$Date = NULL
sentiment_t3$Parent.ID = NULL

sentiment_t3_ordered = sentiment_t3[order(sentiment_t3$compound),]

negative_t3 = sentiment_t3[sentiment_t3$compound <= -0.5,]
positive_t3 = sentiment_t3[sentiment_t3$compound >= 0.5,]

tm_ = unique(as.matrix(getTermMatrixWithTM(positive_t3, -1, sparsity = 0.98498730956292, weightTfIdf)))
tm_ = tm_[rowSums(tm_) > 0,]
dim(tm_)

d1 = getOverallMostFrequentWords(tm_)
barplot(d1[1:10,]$freq, las = 2, names.arg = d1[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

K = 10
model = LDA(tm_, k = K, method = "Gibbs", control = list(burnin = 500, thin = 100, iter = 4000, alpha = 0.1))
printTermsPerTopic(model, 10, -1)


######
library(tidytext)

text = sentiment_t3[sentiment_t3$Author == "philip1201",]$Content
text = "Broken 900,000 now Averaging about 1.3k per minute "

result = tokenizers::tokenize_words(text)
for (i in result[[1]]) {
  scores = analyser$polarity_scores(i)
  print(paste(i, scores$comp))
}

########

T_13 = t_3 %>%
  group_by(Author) %>%
  summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
  arrange(desc(nr_of_posts))

r = apply(as.data.frame(T_13$text), 1, function(x) analyser$polarity_scores(x))
sentiment_T_13 = cbind(T_13, data.frame(matrix(unlist(r), nrow=length(r), byrow=T, dimnames = list(NULL, c("neg", "neu", "pos", "compound")))))

sentiment_T_13$NofComments = NULL
sentiment_T_13$Date = NULL
sentiment_T_13$Parent.ID = NULL
sentiment_T_13$Content = sentiment_T_13$text 
sentiment_T_13$text = NULL

negative_t3 = sentiment_T_13[sentiment_T_13$compound <= -0.8,]
positive_t3 = sentiment_T_13[sentiment_T_13$compound >= 0.8,]

tm_ = unique(as.matrix(getTermMatrixWithTM(negative_t3, -1, sparsity = 0.98498730956292, weightTfIdf)))
tm_ = tm_[rowSums(tm_) > 0,]
dim(tm_)

d1 = getOverallMostFrequentWords(tm_)
barplot(d1[1:10,]$freq, las = 2, names.arg = d1[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#####

result = apply(as.data.frame(sentiment_t3_ordered$compound), 1, function(x) {
  if(x >= 0.05)
    return('positive')
  else if (x > -0.05 & x < 0.05) {
    return('neutral')
  }
  else
    return('negative')
})
sentiment_t3_ordered = cbind(sentiment_t3_ordered, result)

print(sentiment_t3_ordered[sentiment_t3_ordered$Author == "mguzmann" & sentiment_t3_ordered$Submission.ID == "5enks1", "Content" ])

r = apply(T_[,'text'], 1, function(x) analyser$polarity_scores(x))
sentiment_T_ = cbind(T_, data.frame(matrix(unlist(r), nrow=length(r), byrow=T, dimnames = list(NULL, c("neg", "neu", "pos", "compound")))))



text = (t_3[t_3$Author == "londonagain",]$Content[1])
print(text)

# The positive, neutral and negative scores represent the proportion of the text that falls inside these 
# whilst the compound score represents the sum of all the lexicon ratings where +1 represents most positive
# and -1 represents most negative.


