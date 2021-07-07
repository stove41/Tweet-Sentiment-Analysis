#Stephen Johnson
#109656410
#Assignment Week 13
#12/2/2020

library("rtweet")
library('dplyr')

setwd('C:/Users/Steve/dev/Grad School/ISMG6470/week13/homework/')

#Twitter scraping
rm(list=ls())

app_name <- "week13_isgm6470"
consumer_key <- "-------"
consumer_secret <- "---------"

create_token(app=app_name, consumer_key=consumer_key, 
             consumer_secret = consumer_secret, set_renv = TRUE)


biden_tweets <- get_timeline(c("JoeBiden"), n=3200)
trump_tweets = get_timeline(c("realDonaldTrump"), n=3200)

b_tweets = biden_tweets %>% select(created_at, text)
t_tweets = trump_tweets %>% select(created_at, text)

tweet_df = merge(data.frame(b_tweets, row.names=NULL), data.frame(t_tweets, row.names=NULL), by=0, all=TRUE)
tweet_df = tweet_df %>% select(created_at.x, text.x, created_at.y, text.y)
names(tweet_df) = c('biden_timestamp', 'biden_tweet', 'trump_timestamp', 'trump_tweet')

write.csv(tweet_df, 'C:/Users/Steve/dev/Grad School/ISMG6470/week13/homework/tweet_df.csv')

##
#Please restart R session here. If you load qdap package while rtweet package is loaded, the polarity function
#Has an error relating to Openssl a dependency for rtweet.
##


#Sentiment analysis
library('ggplot2')
library('ggthemes')
library('tm')
library('wordcloud')
library('qdap')
library("dplyr")
library("cowplot")
library('stringr')

setwd('C:/Users/Steve/dev/Grad School/ISMG6470/week13/homework/')
#Load tweets from csv.
tweet_df = read.csv('C:/Users/Steve/dev/Grad School/ISMG6470/week13/homework/tweet_df.csv')

#Select bidens tweets, clean them and generate polarities for each tweet.
b_tweets = tweet_df$biden_tweet
b_tweets = b_tweets %>% removePunctuation() %>% stripWhitespace()
b_tweets_pol = b_tweets %>% polarity()
#append polarities on tweet_df
tweet_df$b_polarity = b_tweets_pol$all$polarity

#Create histogram of Bidens tweet polarity distribution.
plot1 = ggplot(b_tweets_pol$all, aes(x=polarity, y=..density..)) + 
  theme_gdocs() + geom_histogram(binwidth=.15, fill='darkred', colour='gray60', size=.3) + 
  geom_density(size=.55)

#Select trumps tweets, clean them and generate polarities for each tweet.
t_tweets = tweet_df$trump_tweet
t_tweets = t_tweets %>% removePunctuation() %>% stripWhitespace()
t_tweets_pol = t_tweets %>% polarity()
#append polarities on tweet_df
tweet_df$t_polarity = t_tweets_pol$all$polarity

#Create histogram of trumps tweet polarity distribution.
plot2 = ggplot(t_tweets_pol$all, aes(x=polarity, y=..density..)) + 
  theme_gdocs() + geom_histogram(binwidth=.15, fill='darkred', colour='gray60', size=.3) + 
  geom_density(size=.55)

#Plot a histogram of the distribution of polarity for trump and biden's tweets.
cowplot::plot_grid(plot1, plot2, labels = c('Biden', 'Trump'))

#subset positive and negative tweets for both trump and biden to create word clouds.
b_pos_tweets = subset(tweet_df$biden_tweet, tweet_df$b_polarity > 0)
b_neg_tweets = subset(tweet_df$biden_tweet, tweet_df$b_polarity < 0)
t_pos_tweets = subset(tweet_df$trump_tweet, tweet_df$t_polarity > 0)
t_neg_tweets = subset(tweet_df$trump_tweet, tweet_df$t_polarity < 0)

#collapse all positive and negative tweets into one vector each.
b_pos_tweets = paste(b_pos_tweets, collapse = ' ')
b_neg_tweets = paste(b_neg_tweets, collapse = ' ')

#combine positive and negative tweets.
b_all_tweets = c(b_pos_tweets, b_neg_tweets)

#build a corpus that contains two docs: (1) positive terms, and (2) negative terms 
b_corpus = VCorpus(VectorSource(b_all_tweets))
## build a term-doc-matrix
b_all_tdm = TermDocumentMatrix(b_corpus, control=list(removePunctuation=TRUE, 
                                                       stopwords(kind='en')))
#convert to matrix from tdm.
b_all_tdm_m = as.matrix(b_all_tdm)
#set column names on matrix
colnames(b_all_tdm_m) = c('positive', 'negative')

## draw a comparison word cloud
par(mar=c(0,0,0,0))
wordcloud::comparison.cloud(b_all_tdm_m, max.words=50, 
                            colors=c('darkgreen', 'darkred'))

#Collapse all positive and negative tweets into one vector each.
t_pos_tweets = paste(t_pos_tweets, collapse = ' ')
t_neg_tweets = paste(t_neg_tweets, collapse = ' ')

#Combine positive and negative tweets.
t_all_tweets = c(t_pos_tweets, t_neg_tweets)

## build a corpus that contains two docs: (1) positive terms, and (2) negative terms 
t_corpus = VCorpus(VectorSource(t_all_tweets))
## build a term-doc-matrix
t_all_tdm = TermDocumentMatrix(t_corpus, control=list(removePunctuation=TRUE, 
                                                       stopwords(kind='en')))
#convert to matrix from tdm.
t_all_tdm_m = as.matrix(t_all_tdm)
#set column names on matrix
colnames(t_all_tdm_m) = c('positive', 'negative')

#draw a comparison word cloud
par(mar=c(0,0,0,0))
wordcloud::comparison.cloud(t_all_tdm_m, max.words=50, 
                            colors=c('darkgreen', 'darkred'))



