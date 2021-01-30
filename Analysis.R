## LOADING THE PACKAGES
library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(scales)
library(gt)

# DOWNLOAD THE TWEETS
# WITH RETWEET
masterways_tweets_RT = search_tweets(q='#masterwaysbrandlaunch', n=1000, include_rts = TRUE)
nrow(masterways_tweets_RT)
save_as_csv(masterways_tweets_RT, "masterways_tweets_RT")

# WITHOUT RETWEET
masterways_tweets = search_tweets(q='#masterwaysbrandlaunch', n=1000, include_rts = FALSE)
save_as_csv(masterways_tweets, "masterways_tweets")