

# Reading the Submissions Data
readSubmissions <- function() {
  diffusions_submissions <- read.csv("C:/Users/andre/Desktop/M1 Internship/Data/diffusions_submissions_extra.csv", stringsAsFactors = FALSE)
  diffusions_submissions$Flair = NULL
  diffusions_submissions$Url = NULL
  diffusions_submissions$Permalink = NULL
  
  diffusions_submissions[diffusions_submissions$Selftext == "NaN", 7] = NA_character_
  
  diffusions_submissions$Content = ifelse(is.na(diffusions_submissions$Selftext), 
                                          paste(diffusions_submissions$Title, " "), paste(diffusions_submissions$Title, diffusions_submissions$Selftext, sep=" ")) 
  
  diffusions_submissions$Title = NULL
  diffusions_submissions$Selftext = NULL
  
  diffusions_submissions[diffusions_submissions$Author == "NaN", 2] = NA_character_
  
  diffusions_submissions[,4] = as.POSIXct(diffusions_submissions[,4], format = "%m/%d/%Y %H:%M",tz="UTC")
  diffusions_submissions = na.omit(diffusions_submissions)
  
  colnames(diffusions_submissions)[colnames(diffusions_submissions)=="Post.ID"] <- "Submission.ID"
  colnames(diffusions_submissions)[colnames(diffusions_submissions)=="Total.No..of.Comments"] <- "NofComments"
  return(diffusions_submissions)
}

readComments <- function() {
  diffusions_comments <- read.csv("C:/Users/andre/Desktop/M1 Internship/Data/diffusions_comments_extra.csv", stringsAsFactors = FALSE)
  
  diffusions_comments$Permalink = NULL
  
  diffusions_comments$Submission.ID = substring(diffusions_comments$Submission.ID, 4)
  diffusions_comments$Parent.ID = substring(diffusions_comments$Parent.ID, 4)
  
  diffusions_comments[,5] = as.POSIXct(diffusions_comments[,5], format = "%m/%d/%Y %H:%M",tz="UTC")
  
  colnames(diffusions_comments)[colnames(diffusions_comments)=="Body"] <- "Content"
  
  diffusions_comments = na.omit(diffusions_comments)
  
  return(diffusions_comments)
}

mergeSubmissionsAndComments <- function(diffusions_submissions, diffusions_comments) {
  posts = smartbind(diffusions_comments, diffusions_submissions)
  posts[,5] = as.Date(posts[,5])
  posts = posts[order(posts$Date), ]
  return(posts)
}

preprocessText <- function(docs) {
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("will", "can", "like", "just", "get")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  return(docs)
}

########## Building a tree ###################
library(data.tree)

buildTree <- function(diffusion) {
  tmp = subset(posts, Submission.ID == diffusion)
  tmp = tmp[order(tmp$Date), ]
  
  root = tmp[is.na(tmp$Parent.ID),]
  tmp = rbind(root, tmp)
  
  tmp = tmp[-c(dim(tmp)[1]),]
  dest = ifelse(is.na(tmp$Comment.ID), tmp$Submission.ID, tmp$Comment.ID)
  res = data.frame("from" = tmp$Parent.ID, "to" = dest)
  tree <- as.Node(res[-1,],mode = "network")
  print(tree)
  #return(tree)
}

getMostFrequentWords <- function(termMatrix, timePeriod) {
  m <- t(as.matrix(termMatrix))
  
  t1 = sort(m[,timePeriod], decreasing = TRUE)
  d1 = data.frame(word = names(t1),freq=t1)
  d1 = subset(d1, word != "dont" )
  return(d1)
}

getOverallMostFrequentWords <- function(termMatrix) {
  m <- t(as.matrix(termMatrix))
  
  t1 = sort(rowSums(m1), decreasing = TRUE)
  d1 = data.frame(word = names(t1),freq=t1)
  d1 = subset(d1, word != "dont" )
  return(d1)
}

plotWordCloud <- function(d1, wc_name) {
  png(wc_name, width=1000,height=800)
  par(mfrow=c(1,2))
  set.seed(1234)
  wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  barplot(d1[1:10,]$freq, las = 2, names.arg = d1[1:10,]$word,
          col ="lightblue", main ="Most frequent words",
          ylab = "Word frequencies")
  
  dev.off()
}



getCosineSimilarity <- function(X) {
  cos.sim <- function(ix) {
    A = X[ix[1],]
    B = X[ix[2],]
    return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
  } 
  n <- nrow(X) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  C <- matrix(apply(cmb,1,cos.sim),n,n)
  return(C)
}

plotTSNE <- function(tsne, m_adj_uniq) {
  hover_text <- apply(m_adj_uniq, 1, function(x) {
    n <- names(x)
    t <- paste(n, x, sep = ": ", collapse = "<br>")
    return(t)
  }
  )
  plotdata <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2],
                         hover_text = hover_text)
  plt2 <- ggplot(plotdata) + 
    geom_point(aes(x = tsne_x, y = tsne_y, text = hover_text))
  
  ggplotly(plt2, width = 1400, height = 700)
}

lseq(0.9, 1, 5)
lseq <- function(from, to, length.out) {
  exp(seq(log(from), log(to), length.out = length.out))
}

performTSNE <- function(t) {
  
  for (period in 1:7) {
    T_period = t[[period]] %>%
      group_by(Author) %>%
      summarise( nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
      arrange(desc(nr_of_posts))
    
    init = textTinyR::sparse_term_matrix$new(vector_data = T_period$text, file_data = NULL, document_term_matrix = TRUE)
    
    tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                          remove_numbers = T, trim_token = T, split_string = T, 
                          stemmer = "porter2_stemmer",
                          split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                          language = "english", min_num_char = 3, max_num_char = 100,
                          print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                          threads = 6, verbose = T)
    
    for (sparsity in lseq(0.9, 1, 6)) {
      print(paste("Sparsity", sparsity))
      
      m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = sparsity))
      m_adj_uniq = unique(m_adj)
      
      
      png("T1_6807_words_embedd.png", width = 3200, height = 1800, res = 300)
      par(mfrow=c(1,3))
      tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=5, verbose=TRUE, max_iter = 2000)
      plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 5",xlab="Dim1", ylab = "Dim2")
      
      tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000)
      plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 30",xlab="Dim1", ylab = "Dim2")
      
      tsne = Rtsne(scale(m_adj_uniq),  dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000)
      plot(tsne$Y[,1], tsne$Y[,2], main="perplexity = 50",xlab="Dim1", ylab = "Dim2")
      dev.off()
      
      
    }
    
    
    
    
  }
  
  
  
}
