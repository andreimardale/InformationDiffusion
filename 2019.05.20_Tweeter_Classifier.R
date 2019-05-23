library("rjson")

json_data <- fromJSON(file="../Data/brexit-sample-20160506-annotated-full.json")

output <- matrix(ncol=4, nrow=length(json_data))

for(i in 1:length(json_data)){
  output[i,1] <- json_data[[i]][["id"]]
  output[i,2] <- json_data[[i]][["text"]]
  output[i,3] <- json_data[[i]][["sentiment"]]
  output[i,4] <- json_data[[i]][["strength"]]
}

output <- data.frame(output)
colnames(output) = c("id", "text", "sentiment", "strength")

#build a corpus and specify the source to be character of vectors
#a corpus is a collection of written texts
tweetCorpus = twitter.preprocessTweets(output$text)
class(tweetCorpus)


twitterDocMatrix <- DocumentTermMatrix(tweetCorpus, control = list(minWordLength = 3))
your_dtm = twitter.selectFeatures(twitterDocMatrix,minfreq = 1)


output$sent = 2
output[output$sentiment == "stay",]$sent = 0
output[output$sentiment == "leave",]$sent = 1
output$sentiment = NULL
output$strength = NULL

your_target <- factor(output$sent)

library('caret')
library('e1071')

set.seed(123)
train_rows <- createDataPartition(your_target, p=0.75) # for 75% training set

dtm_train <- your_dtm[train_rows$Resample1,]
y_train <- your_target[train_rows$Resample1] 

dtm_test <- your_dtm[-train_rows$Resample1,] 
y_test <- your_target[-train_rows$Resample1] 

tr_ctrl <- trainControl(method='repeatedcv', number=5, repeats=3, returnResamp='none') 

library(parallel)
library(doParallel)
use_cores <- detectCores()-1
cl <- makeCluster(use_cores)
registerDoParallel(cl)
nb <- train(x = dtm_train, y = y_train, method='nb', trControl = tr_ctrl)
stopCluster(cl)


predicted.classes =  predict(nb, dtm_test)
mean(predicted.classes ==  your_target)

View(predicted.classes)

auc_train <- roc(y_train, predict(nb, newdata = dtm_train, type='raw') )
auc_test <- roc(y_test, predict(nb, newdata = dtm_test, type='raw') )
writeLines(paste('AUC using nb:', round(auc_train$auc,4),'on training/validation set', round(auc_test$auc,4),'on test set.'))




##### Utility Functions ####
#This mehod removes all the URLs from a tweet
removeURL <- function(tweetText) {
  gsub("http[[:alnum:][:punct:]]+", "", tweetText)
}

#This method preprocess the tweets
twitter.preprocessTweets <- function(tweets) {
  #get rid of latin words by cnverting to utf-8
  tweets = iconv(tweets, "LATIN1", "UTF-8",sub="")
  tweets = iconv(tweets, "LATIN2", "UTF-8",sub="")
  #Create corpus out of the tweets
  tweetCorpus <- Corpus(VectorSource(tweets),readerControl=list(language="en"))
  #Convert to lowercase
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(tolower))
  #Remove the URLs from the tweet
  tweetCorpus <- tm_map(tweetCorpus, removeURL)
  #Removes all the punctuation
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  #Removes all the numbers
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  #removes the stop words 
  tweetCorpus <- tm_map(tweetCorpus, removeWords, c(stopwords("english"),"rt","http","retweet"))
  #Stem the document using SnowballC
  tweetCorpus <- tm_map(tweetCorpus, stemDocument)
  #Convert to plain text document
  # tweetCorpus <- tm_map(tweetCorpus, PlainTextDocument,language="english")
  return(tweetCorpus)
}

#THis method is used to select features from the document term matrix of the tweets.
#The logic used is to eliminate all the attributes that have the total sum of the 
#column less than a given threshold as specified by minFreq paramater
twitter.selectFeatures <- function(twitterDocMatrix,minfreq = 3) {
  #Get terms with frequency as minFreq
  frequentTerms =findFreqTerms(twitterDocMatrix, lowfreq=minfreq)
  dm.matrix = as.matrix(twitterDocMatrix)
  #selct the subset of features
  dm.matrix =  dm.matrix[,frequentTerms]
  return(dm.matrix)
}

twitter.appendClass <- function(doc.matrixToModify) {
  doc.dataFrame = as.data.frame(doc.matrixToModify)
  doc.dataFrame$Class = ''
  #Append love class
  doc.dataFrame[doc.dataFrame$love>0,'Class'] = 'love'
  #Append hate class
  doc.dataFrame[doc.dataFrame$hate>0,'Class'] = 'hate'
  doc.dataFrame[doc.dataFrame$Class=='','Class'] = 'love'
  return(doc.dataFrame)
}

####
