

# Reading the Submissions Data
readSubmissions <- function() {
  diffusions_submissions <- read.csv("../Data/diffusions_submissions_extra.csv", stringsAsFactors = FALSE)
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
  diffusions_comments <- read.csv("../Data/diffusions_comments_extra.csv", stringsAsFactors = FALSE)
  
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

splitInTimeframes <- function(posts, N, equal = F) {
  if (equal) {
    pts_N_cut = N
    labels_N_cut = paste0("T", 1:N)
  }
  else {
    if (N == 7) {
      pts_N_cut =  c(as.Date("2015-11-16", format="%Y-%m-%d"), as.Date("2016-06-24", format="%Y-%m-%d"),
                     as.Date("2017-03-30", format="%Y-%m-%d"), as.Date("2018-07-07", format="%Y-%m-%d"),
                     as.Date("2018-11-26", format="%Y-%m-%d"), as.Date("2019-03-21", format="%Y-%m-%d"),
                     as.Date("2019-03-30", format="%Y-%m-%d"), as.Date("2019-04-06", format="%Y-%m-%d"))
      labels_N_cut = c("T1", "T2", "T3", "T4", "T5", "T6", "T7")
    } else if (N == 16) {
      pts_N_cut = c(as.Date("2015-11-16", format="%Y-%m-%d"), as.Date("2016-06-25", format="%Y-%m-%d"),
                    as.Date("2016-07-14", format="%Y-%m-%d"), as.Date("2016-07-28", format="%Y-%m-%d"),
                    as.Date("2016-12-08", format="%Y-%m-%d"), as.Date("2017-01-27", format="%Y-%m-%d"),
                    as.Date("2017-03-30", format="%Y-%m-%d"), as.Date("2017-06-20", format="%Y-%m-%d"),
                    as.Date("2018-07-09", format="%Y-%m-%d"), as.Date("2018-09-22", format="%Y-%m-%d"),
                    as.Date("2018-11-16", format="%Y-%m-%d"), as.Date("2018-11-26", format="%Y-%m-%d"),
                    as.Date("2019-01-16", format="%Y-%m-%d"), as.Date("2019-03-15", format="%Y-%m-%d"),
                    as.Date("2019-03-22", format="%Y-%m-%d"), as.Date("2019-03-30", format="%Y-%m-%d"),
                    as.Date("2019-04-06", format="%Y-%m-%d"))
      labels_N_cut = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16")
    } else if (N == 15) {
      pts_N_cut = c(as.Date("2015-11-16", format="%Y-%m-%d"), as.Date("2016-06-25", format="%Y-%m-%d"),
                    as.Date("2016-07-14", format="%Y-%m-%d"), as.Date("2016-12-08", format="%Y-%m-%d"),
                    as.Date("2017-01-27", format="%Y-%m-%d"), as.Date("2017-03-30", format="%Y-%m-%d"), 
                    as.Date("2017-06-20", format="%Y-%m-%d"), as.Date("2018-07-09", format="%Y-%m-%d"),
                    as.Date("2018-09-22", format="%Y-%m-%d"), as.Date("2018-11-16", format="%Y-%m-%d"), 
                    as.Date("2018-11-26", format="%Y-%m-%d"), as.Date("2019-01-16", format="%Y-%m-%d"), 
                    as.Date("2019-03-15", format="%Y-%m-%d"), as.Date("2019-03-22", format="%Y-%m-%d"), 
                    as.Date("2019-03-30", format="%Y-%m-%d"), as.Date("2019-04-06", format="%Y-%m-%d"))
      labels_N_cut = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13", "T14", "T15")
    }
  }
  
  dates = cut.Date(posts$Date, breaks=pts_N_cut, labels = labels_N_cut)
  print(summary(dates))
  t = split(posts, dates)
  return(t)
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
  m1 <- t(as.matrix(termMatrix))
  
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
  
  ggplotly(plt2, width = 3200, height = 1600, res = 300)
}

lseq <- function(from, to, length.out) {
  exp(seq(log(from), log(to), length.out = length.out))
}

performTSNE <- function(t, P, S, tf_idf = TRUE) {
  root = ifelse(tf_idf == TRUE, "Tf-Idf", "Tf")
  
  for (period in P) {
    print(paste("Period", period))
    
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
                          print_every_rows = 100000, normalize = NULL, tf_idf = tf_idf, 
                          threads = 6, verbose = T)
    
    for (sparsity in S) {
      print(paste("Sparsity", sparsity))
      
      m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = sparsity))
      m_adj_uniq = unique(m_adj)
      m_adj_uniq = m_adj_uniq[rowSums(m_adj_uniq) > 0,]
      
      
      cosine = cosine(t(m_adj_uniq))
      
      set.seed(1)
      tsne = Rtsne(cosine, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2000, is_distance = T)
      id = paste(paste0(paste(root, "T", sep = " "), period), dim(m_adj_uniq)[2]  , "words", "sparsity", sparsity, sep = "_")
      title = paste0(id, ".png")
      
      png(title, width = 1600, height = 1600, res = 300)
      plot(tsne$Y[,1], tsne$Y[,2], main=paste(root, "NoScale", "Cosine", "NoPCA", "t-SNE", sep = "-"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
      dev.off()
        
      
      
    }
  }
}

applyKMeans <- function(tm_uniq, timeframe) {
  scal_dat = ClusterR::center_scale(tm_uniq)
  kmed = ClusterR::Cluster_Medoids(scal_dat, clusters = 2,
                                   distance_metric = "pearson_correlation", threads = 6, swap_phase = TRUE,
                                   fuzzy = FALSE, verbose = F, seed = 1)

  t_1 = sort(colSums(tm_uniq[kmed$clusters == 1, ]), decreasing = TRUE)
  d_1 = data.frame(word = names(t_1),freq=t_1)

  t_2 = sort(colSums(tm_uniq[kmed$clusters == 2, ]), decreasing = TRUE)
  d_2 = data.frame(word = names(t_2),freq=t_2)
  
  plotWordCloud(d_1,  paste0(paste("wordcloud_cluster_1", timeframe, sep = "_" ), ".png"))
  plotWordCloud(d_2,  paste0(paste("wordcloud_cluster_2", timeframe, sep = "_" ), ".png"))
  
  return(kmed$clusters)
}

getTermMatrix <- function(T1, sparsity) {
  init = textTinyR::sparse_term_matrix$new(vector_data = T1$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = T, 
                        threads = 6, verbose = T)
  tm_1 <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = sparsity))
  print(paste0("Dimension of the term matrix ", paste(dim(tm_1)[1], dim(tm_1)[2], sep = ",") ))
  tm_1_uniq = unique(tm_1)
  
  print(paste0("Dimension of the unique rows term matrix ", paste(dim(tm_1_uniq)[1], dim(tm_1_uniq)[2], sep = ",")))
  
  return(tm_1_uniq)
}

getTransition <- function(authorNumber, tm_1, tm_14) {
  
  
  encoding.T1 = tm_1[as.integer(index.T1),]
  
  tm_1_uniq = unique(tm_1)
  position.T1 = 0
  
  for (i in 1:dim(tm_1_uniq)[1]) {
    D = as.vector(encoding.T1)
    E = as.vector(tm_1_uniq[i, ])
    
    if (all(length(D)==length(E)) && all(D==E)) {
      position.T1 = i
    }
  }
  
  #########
  
  
  encoding.T14 = tm_14[as.integer(index.T14),]
  
  tm_14_uniq = unique(tm_14)
  position.T14 = 0
  
  for (i in 1:dim(tm_14_uniq)[1]) {
    D = as.vector(encoding.T14)
    E = as.vector(tm_14_uniq[i, ])
    
    if (all(length(D)==length(E)) && all(D==E)) {
      position.T14 = i
    }
  }
  
  #########
  
  colors = c(rep(1, dim(tm_1_uniq)[1]), rep(2, dim(tm_14_uniq)[1]))
  
  title = paste0(paste("tsne_T1_T14_translation_auth", authorNumber, sep = "_"), ".png")
  
  png(title, width = 1800, height = 1800, res = 300)
  plot(tsne$Y[,1], tsne$Y[,2],main="T1 vs T14; perplexity = 50",xlab="Dim1", ylab = "Dim2", col = adjustcolor(colors, alpha=0.25), pch=16)
  points(tsne$Y[position.T1,1], tsne$Y[position.T1,2], col = 3, lwd = 2)
  points(tsne$Y[position.T14 + dim(tm_1_uniq)[1] - 1, 1], tsne$Y[position.T14 + dim(tm_1_uniq)[1] - 1, 2], col = 4, lwd = 2)
  legend("topleft",c("T1","T14", "Start", "End"), cex=.8, col=c("black","red", 3, 4),pch=c(16,16, 1, 1))
  dev.off()
  
}

plotLeaders <- function(authorNumber, tm, timeframe) {
  
  encoding = tm[as.integer(authorNumber),]
  
  tm_uniq = unique(tm)
  position = 0
  
  for (i in 1:dim(tm_uniq)[1]) {
    D = as.vector(encoding)
    E = as.vector(tm_uniq[i, ])
    
    if (all(length(D)==length(E)) && all(D==E)) {
      position = i
    }
  }
  
  if (timeframe == "T1") {
    colour = "green"
  }
  else {
    colour = "blue"
    position = position + dim(tm_1_uniq)[1] - 1
  }
  
  points(tsne$Y[position,1], tsne$Y[position,2], col = colour, lwd = 2)
  
}
