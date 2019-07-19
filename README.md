# InformationDiffusion
M1 - Internship - Lab HC - Information Diffusion in Online Communities

##Main contributions##
1) Longitudinal analysis of discussions around Brexit on two social media platforms: Twitter and Reddit;
2) Tool for visualizing the dynamics of discussion topics and trajectory of users;
3) Prediction of future political stance based on features defined using the structure
online diffusions

##Datasets##
https://www.dropbox.com/s/xx9y6setca7vz0q/Data.zip?dl=0
1) data_circulate/data_dt_brexittweets.Rdata - the full Twitter dataset used for training the NB classifier
2) data_circulate/data_dt_brexittweets_sample.Rdata - a sample of the Twitter dataset used for training the NB classifier
3) data_tabulation - results of the prediction of neutral users (not important)
4) correct_model_no_hashtags.rds - the NB classifier used for labelling the political stance
5) diffusions_comments_extra.csv - the Reddit dataset (the comments)
6) diffusions_submissions_extra.csv - the Reddit dataset (the initial reddits (the roots))
7) Fx_improved_data.RData - the training dataset build using features extracted from the structure of the diffusions
8) nb_with_hashtags_july.rds - a NB classifier trained on the Twitter dataset, but aimed to label Twitter data (it takes into account Twitter specific information such as the hashtags or mentions)



##Content##
###Code Files###
1) 2019.04.02_statistics.R
	- Read the input Reddit dataset
	- Split the posts set into time-periods
	- Aggregating all textual content of the dataset and build a DTM, then build word clouds for each period and for the whole interval
	- Plot heatmap with the evolution of the topics in the 15 time-periods
	- Plot two timeframes in the same graphic
	- Plot transitions of common users between different time periods
	- Find and Plot the Leaders of two different time periods. (leader = user with high number of posts)
	- Structural Analysis of the Reddit dataset - density of posts, no of unique users, no of submissions, posts etc., CCDF plots for the number of comments per diffusion

2) 2019.04.19_distances_for_tsne.R
	- different tests on how we should represent the users using T-SNE in the Terms Space (w or w/o PCA, w or w/o tf-idf)
	- not crucial to understand as they don't affect the Machine Learning part.

3) 2019.04.23_PCA_on_covariance_matrix.R
	- representing the users in the term space using PCA instead of T-SNE - turns out not to be so successfull

4) 2019.04.25_LDA_users_and_terms_in_topics_space.R
	- split the users in periods, aggregate users replies, build DTMs, apply LDA, represent the users in a 10 dimensional space coresponding to the probability distribution of topics over the users' discourses
	- print most important terms of each topics
	- apply T-SNE to reduce dimensionality then plot users and terms in the same 2D space. 

5) 2019.04.26_CLUSTERING_users_in_word_space.R
	- split the users in periods, aggregate users replies, build DTMs, use T-SNE to reduce dimensionality, plot and perform clustering of the users in the terms space (using k-medoids and hdbscan)


6) 2019.04.26_TSNE_users_in_words_space.R
	- split the users in periods, aggregate users replies, build DTMs, use T-SNE to reduce dimensionality, plot users

7) 2019.04.29_LDA_k_cross_validation_to_detect_topic_numbers.R
	- for each time period perform k-fold CV to detect the optimal number of topics that should be sought via LDA (experimental)

8) 2019.05.01_CLUSTERING_users_in_topic_space.R
	- split the users in periods, aggregate users replies, build DTMs, apply LDA, obtain the representation of the users in topic space, use T-SNE to reduce dimensionality, plot and perform clustering of the users in the terms space (using k-medoids and hdbscan)

9) 2019.05.06_LDA_distances_between_topics.R
	- for a certain period, aggregate users texts and apply LDA to obtain the most important topics
	- use KL divergence to compute the distance between the topics and build a heatmap with these distances

10) 2019.05.07_LDA_parallel_distances_between_topics.R
	- same as 9) but done in parallel

11) 2019.05.08_LDA_determine_subjects_of_clusters.R
	- represent users in the topic space using LDA and cluster the users in this new space using K-medoids
	- determine the most important clusters and get the thematics in each of them


12) 2019.05.15_SENTIMENT_ANALYSIS_SentimentR.R
	- try to polarize the users using sentiment analysis using SentimentR library - did not work. 

13) 2019.05.15_SENTIMENT_ANALYSIS_Vader.R
	- try to polarize the users using sentiment analysis using VadeR library - did not work. 

14) 2019.05.24_Twitter_Political_Stance_Detector.R
	- using the Twitter Dataset and Ken Benoit's methodology, a political stance labeler is trained
	- aggregate users tweets, filter out users who do not use targetted hashtags (hashes which refer to brexit or against brexit)
	- filter users who do not send at least a min threshold number of tweets
	- compute a leave score based on the difference between the number of leave hashtags and remain hashtags of each users and sort all users acording to this score
	- keep the top 10% and bottom 10% as the training set
	- train a Naive Bayes classifier on this smaller corpus

15) 2019.07.16_Reddit_Classify_F0.R
 	- manufacture textual features in order to train the political stance predictor on the Reddit Dataset
 	- aggregate every users replies and build a DTM, then obtain the 100 most frequent overall terms - this will be the features vocabulary
 	- for every consecutive 2 time-periods, get common users between the 2 periods: from the first period get the training features (the frequencies of the terms in the features vocabulary) for every common user and the current political stance. From the second period, obtain the future political stance
 	- after the first phase, filter the resulting training set: eliminate training elements who have transitions from Neutral to Neutral and appear once (the training features)
 	- save the resulting dataset in a file named: F0_improved_data.csv

16) 2019.05.20_Reddit_Classify_F1.R
	- build FS1 (which considers the user activity information - no of received replies, no of submitted replies), no of initiated threads and no of submitted comments


17) 2019.06.05_Reddit_Classify_F2.R
	- build FS2 (which considers the user activity per group), no of received replies from each group, no of submitted replies to each group, no of initiated threads and no of submitted comments


18) 2019.05.30_Reddit_Classify_F3.R
	- build FS3 (which considers the structure of the diffusions a user is part of - ratio of users from each group in the diffusions)

19) 2019.06.11_Reddit_Build_Mixed_Features.R
	- different combination of the above feature sets


20) 2019.06.17_Tabulation_Of_Users_Per_period.R
	- (experimental) - for detecting the ratio of neutral users who have translations to the pro / against brexit side
 
21) 2019.07.04_Twitter_Dataset_Statistics.R
	- the same exploratory analyis which was performed on the Reddit dataset is now performed on the Twitter dataset.

22) 2019.07.10_Twitter_FS1_Building.R
	- building FS1 - Twitter version - initiated threads = original tweets, comments = retweets

23) 2019.07.16_Twitter_FS2_Building.R
	- building FS2

24) Python/ReditCrawler/crawlReddit.py
	- the python script used for obtaining the Reddit dataset

25) Python/RunClassifiers - the models trained in Python for performing the prediction


###Reports###
- contains the PDFs with the partial reports delivered thorughout the internship 

###Final Documents###
1) M1_Report_MardaleAndrei_LabHC.pdf - final report describing in details this study
2) M1_Presentation_MardaleAndrei_LabHC.pptx - the presentation slides for defending the project
3) M1_Poster_MardaleAndrei_LabHC.pdf - the poster for the 2nd Manutech conference