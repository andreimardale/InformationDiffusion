colPal = c("black", "#B15928", "#B2DF8A", "green3", "blue", "cyan","magenta", "yellow", brewer.pal(n = 12, name = "Paired"))

# Initializing Data
initializeData <- function(N = 15) {
  # Reading the Submissions Data
  diffusions_submissions = readSubmissions()
  
  # Reading the Comments Data
  diffusions_comments = readComments()
  
  # Merging the submissions and the comments
  posts = mergeSubmissionsAndComments(diffusions_submissions, diffusions_comments)
  
  # Split the posts into a list of timeframes defined by the array of cutting points inside this function.
  t = splitInTimeframes(posts, N)
  return(t)
}

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
  
  tmp = cut(posts$Date, 15)
  levels(tmp)
    
  dates = cut.Date(posts$Date, breaks=pts_N_cut, labels = labels_N_cut)
  print(summary(dates))
  t = split(posts, dates)
  return(t)
}

buildTree <- function(diffusion) {
  tmp = subset(posts, Submission.ID == diffusion)
  tmp = tmp[order(tmp$Date), ]
  
  root = tmp[is.na(tmp$Parent.ID),]
  # tmp = rbind(root, tmp)
  # 
  # tmp = tmp[-c(dim(tmp)[1]),]
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

plotTSNE <- function(tsne, m_adj_uniq, cr = "black") {
  
  hover_text <- lapply(1:nrow(m_adj_uniq), function(position, df) {
    x = df[position,]
    x = (head(sort(x, T), 10))
    n <- names(x)
    t <- paste(n, x, sep = ": ", collapse = "<br>")
    t = paste(t, paste("reply", rownames(df)[position], sep = ":"), sep = "<br>")
    return(t)
    
  }, m_adj_uniq)
  
  # hover_text <- apply(m_adj_uniq, 1, function(x) {
  #   x = (head(sort(x, T), 10))
  #   n <- names(x)
  #   t <- paste(n, x, sep = ": ", collapse = "<br>")
  #   t = paste(t, "cucu", sep = "<br>")
  #   return(t)
  # })
  plotdata <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2],
                         hover_text = hover_text)
  plt2 <- ggplot(plotdata) + 
    geom_point(aes(colour = cr, x = tsne_x, y = tsne_y, text = hover_text))
  
  ggplotly(plt2)
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
                          threads = 3, verbose = T)
    
    for (sparsity in S) {
      print(paste("Sparsity", sparsity))
      
      m_adj <- as.matrix(init$Term_Matrix_Adjust(sparsity_thresh = sparsity))
      m_adj_uniq = unique(m_adj)
      m_adj_uniq = m_adj_uniq[rowSums(m_adj_uniq) > 0,]
      
      
      cosine = cosine(t(m_adj_uniq))
      cosine.dist = 1 - cosine
      after.pca = cmdscale(cosine.dist, 50)
      
      tsne = Rtsne(after.pca, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = F, pca = F)
      
      id = paste(paste0(paste(root, "T", sep = " "), period), dim(m_adj_uniq)[2]  , "words", "sparsity", sparsity, sep = "_")
      title = paste0(id, ".png")
      
      png(title, width = 1600, height = 1600, res = 300)
      plot(tsne$Y[,1], tsne$Y[,2], main=paste(root, "NoScale", "Cosine", "PCA", "t-SNE", sep = "-"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(1, alpha=0.5), pch=16)
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


getTermMatrixWithTM <- function(t, time_frame , sparsity, tfidf = weightTf) {
  if(time_frame != -1){
    T_ = t[[time_frame]] %>%
      group_by(Author) %>%
      summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
      arrange(desc(nr_of_posts))
  }
  else {
    T_ = t %>%
      group_by(Author) %>%
      summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
      arrange(desc(nr_of_posts))
  }
  
  docs = VCorpus(DataframeSource(data.frame(doc_id=row.names(T_), text=T_$text)))
  docs = tm_map(docs, content_transformer(removePunctuation))
  docs <- tm_map(docs, content_transformer(gsub), pattern = "will", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "‘", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "’", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "”", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "“", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "¿", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "„", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "…", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = " ", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "–", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "/", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "@", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "\\|", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "\u2028", replacement = " ", fixed=TRUE)
  
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, c(stopwords("english"),
                                      "ill", "youll", "theyll", "hell", "shell",
                                      "ive", "youve", "hes", "shes", "weve", "youv", "theyve",
                                      "will", "wont", "wouldnt", "couldnt", "arent", "didnt", "wasnt", "isnt", "dont",
                                      "theyr", "theyv", "etc", "amp", "also",
                                      "gonna", "ing", "whether", "if", "unless", "yes", "no", "or",
                                      "just", "one", "can", "like", "get", "now"))
  docs <- tm_map(docs, stemDocument)
  docs <- tm_map(docs, stripWhitespace)
  

  dtm <- DocumentTermMatrix(docs, control = list(minWordLength = 3, weighting = tfidf, removeNumbers = TRUE, stopwords = TRUE))
  dtm <- removeSparseTerms(dtm, sparsity)
  
  return(Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames = dtm$dimnames))
  
  # return(dtm)
}

getTermMatrixWithTMForOneReply <- function(text, sparsity, tfidf = weightTf) {
  docs = VCorpus(VectorSource(text))
  docs = tm_map(docs, content_transformer(removePunctuation))
  docs <- tm_map(docs, content_transformer(gsub), pattern = "will", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "‘", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "’", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "”", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "“", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "¿", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "„", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "…", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = " ", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "–", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "/", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "@", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "\\|", replacement = " ", fixed=TRUE)
  docs <- tm_map(docs, content_transformer(gsub), pattern = "\u2028", replacement = " ", fixed=TRUE)
  
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, c(stopwords("english"),
                                      "ill", "youll", "theyll", "hell", "shell",
                                      "ive", "youve", "hes", "shes", "weve", "youv", "theyve",
                                      "will", "wont", "wouldnt", "couldnt", "arent", "didnt", "wasnt", "isnt", "dont",
                                      "theyr", "theyv", "etc", "amp", "also",
                                      "gonna", "ing", "whether", "if", "unless", "yes", "no", "or",
                                      "just", "one", "can", "like", "get", "now"))
  docs <- tm_map(docs, stemDocument)
  docs <- tm_map(docs, stripWhitespace)
  
  
  dtm <- DocumentTermMatrix(docs, control = list(minWordLength = 3, weighting = tfidf, removeNumbers = TRUE, stopwords = TRUE))
  dtm <- removeSparseTerms(dtm, sparsity)
  
  return(Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames = dtm$dimnames))
}


getTermMatrix <- function(t, time_frame , sparsity, tfidf = T) {
  T_ = t[[time_frame]] %>%
    group_by(Author) %>%
    summarise(nr_of_posts = n(), text = paste0(Content, collapse = " ")) %>%
    arrange(desc(nr_of_posts))
  
  init = textTinyR::sparse_term_matrix$new(vector_data = T_$text, file_data = NULL, document_term_matrix = TRUE)
  
  tm = init$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = T, remove_punctuation_string = F,
                        remove_numbers = T, trim_token = T, split_string = T, 
                        stemmer = "porter2_stemmer",
                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                        language = "english", min_num_char = 3, max_num_char = 100,
                        print_every_rows = 100000, normalize = NULL, tf_idf = tfidf, 
                        threads = 3, verbose = T)
  tm_adj <- init$Term_Matrix_Adjust(sparsity_thresh = sparsity)
  return(tm_adj)
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

performKMedoids <- function(distMatrix, k, tsne) {
  kmed = pam(distMatrix, k = k, diss = TRUE, keep.diss = TRUE)
  filename = paste0(paste("kmedoids", "k", k, sep = "_"), ".png")
  png(filename, width = 1800, height = 1800, res = 300)
  plot(tsne$Y[,1], tsne$Y[,2],main=paste("kmedoids", "k", k, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colPal[kmed$clustering], alpha=0.5), pch=16)
  dev.off()
  return(kmed)
}

performHDBSCAN <- function(distMatrix, minPts, tsne) {
  dbscan.result = dbscan::hdbscan(distMatrix, minPts = minPts)

  filename = paste0(paste("hdbscan", "minpts", minPts, sep = "_"), ".png")
  png(filename, width = 1800, height = 1800, res = 300)
  plot(tsne$Y[,1], tsne$Y[,2],main=paste("hdbscan", "minpts", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(colPal[dbscan.result$cluster+1], alpha=0.5), pch=16)
  legend("topleft", paste0("Cluster ", sort(unique(dbscan.result$cluster))),
         cex=.8, col=colPal[1 + sort(unique(dbscan.result$cluster))], pch = rep(16,8))
  dev.off()
  return(dbscan.result$cluster)
}

performDBSCANandPlot8BiggestClusters <- function(distMatrix, minPts, tsne) {
  dbscan.result = dbscan::hdbscan(distMatrix, minPts = minPts)
  df = as.data.frame(dbscan.result$cluster)
  
  re7 = df %>%
    group_by(dbscan.result$cluster) %>%
    summarise( nr_of_elems = n()) %>%
    arrange(desc(nr_of_elems)) %>%
    head(n=8)
  
  df2 = df
  for (i in (unique(df$`dbscan.result$cluster`))){
    if (!(i %in% re7$`dbscan.result$cluster`)) {
      df2[df==i]<-1
    }
  }
  for (i in 1:8) {
    df2[df==as.integer(re7[i,1])]<-as.integer(rownames(re7)[i])
  }
  
  png(paste( minPts,"dbscan.png", sep = "-"), width = 1800, height = 1800, res = 300)
  plot(tsne$Y[,1], tsne$Y[,2],main=paste("hdbscan", "minpts", minPts, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(df2$`dbscan.result$cluster`, alpha=0.5), pch=16)
  dev.off() 
  
  # for (cluster in 1: dim(re7)[1]) {
  #   png(paste("detailed", "cluster", cluster, sep = "_"), width = 1600, height = 1600, res = 300)
  #   plot(tsne$Y[df2$`dbscan.result$cluster` == cluster,1], tsne$Y[df2$`dbscan.result$cluster` == cluster,2],main=paste("kmedoids", "cluster", cluster, sep = "_"), xlab="Dim1", ylab = "Dim2", col = adjustcolor(cluster, alpha=0.5), pch=16)
  #   dev.off()
  #   
  #   t_cluster = sort(colSums(tm_[df2$`dbscan.result$cluster`== cluster, ]), decreasing = TRUE)
  #   d_cluster = data.frame(word = names(t_cluster),freq=t_cluster)
  #   
  #   plotWordCloud(d_cluster, paste("wordcloud", "cluster", cluster, sep = "_"))
  # }
  
}

printTermsPerTopic <- function(model, top_n = 10, period) {
  png(paste0(paste0("lda", model@k),"_topics_T", period, ".png"), width = 3200, height = 1800, res = 300)
  
  ap_topics <- tidy(model, matrix = "beta")
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(top_n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = reorder(term, beta))
  
  print(ggplot(data = ap_top_terms, aes(term, beta, fill = factor(topic))) +
    ggtitle("Top 10 words for each discovered topic")+
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip())
  
  dev.off()
}
