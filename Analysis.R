# LOADING THE PACKAGES
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

# PERFORMING THE ANALYSIS
# WITH RETWEET
masterways_tweets_RT$followers_count
masterways_tweets_RT$retweet_followers_count
masterways_tweets_RT$location
masterways_tweets_RT$retweet_followers_count

## plot multiple time series--retweets vs non-retweets
masterways_tweets_RT %>%
  dplyr::group_by(is_retweet) %>%
  ts_plot("hours") +
  ggtitle("Time of tweets and retweets vs frequency") +
  theme_bw()

# PLOT TIME SERIES OF TWEETS FREQUENCY
ts_plot(masterways_tweets_RT, "hours")

# with no retweet
ts_plot(masterways_tweets, "hours")

# FILTERING COLUMNS AM INTERESTED IN
masterways_RT_df <- data.frame(
  tweet = masterways_tweets_RT$text,
  created_at = masterways_tweets_RT$created_at,
  screen_name = masterways_tweets_RT$screen_name,
  isretweet = masterways_tweets_RT$is_retweet,
  retweet_text = masterways_tweets_RT$retweet_text,
  retweet_created_at = masterways_tweets_RT$retweet_created_at,
  retweet_count = masterways_tweets_RT$retweet_count
)
ts_plot(masterways_RT_df)

# users with the most tweets
user_tweets = masterways_RT_df %>%
  filter(isretweet == FALSE)
  
user_tweets %>%
  count(screen_name, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(screen_name = reorder(screen_name, n)) %>%
  ggplot(aes(screen_name, n)) +
  geom_col() +
  ggtitle("users with the most tweets") +
  xlab("screen names") +
  ylab("tweets count")+
  theme_bw() +
  coord_flip()

# users with the most tweets
user_retweets = masterways_RT_df %>%
  filter(isretweet == TRUE)

user_retweets %>%
  count(screen_name, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(screen_name = reorder(screen_name, n)) %>%
  ggplot(aes(screen_name, n)) +
  geom_col() +
  ggtitle("users with the most retweets") +
  xlab("screen names") +
  ylab("retweets count")+
  theme_bw() +
  coord_flip()

# CLEANING THE TWEETS
masterways_RT_df$tweet

# REMOVING LINKS
masterways_RT_df$tweet = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", masterways_RT_df$tweet)

# REMOVE STOP WORDS
data("stop_words")
master_tweets = masterways_RT_df %>% 
  select(tweet) %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words) 

# REMOVE @people
masterways_RT_df$tweet = gsub("@\\w+", "", masterways_RT_df$tweet)

# REMOVE #
masterways_RT_df$tweet = gsub("#\\w+"," ",masterways_RT_df$tweet)

# REMOVE NUMBERS
masterways_RT_df$tweet = gsub("[[:digit:]]"," ", masterways_RT_df$tweet)

# REMOVE EMOJIS
masterways_RT_df$tweet = gsub("[^\01-\x7F]", "" , masterways_RT_df$tweet)

masterways_tweets = masterways_RT_df %>% 
  select(tweet) %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words) 


# MOST FREQUENT WORDS
masterways_tweets %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(prop = n/sum(n)) %>%
  top_n(30) %>%
  ggplot(aes(x = word, y = n)) +
  geom_text(aes(label = percent(prop)),  hjust = -1) +
  geom_col(fill='blue') +
  coord_flip() + 
  ggtitle("the most frequent words in the tweets") +
  theme_bw() +
  xlab("")

# SENTIMENT ANALYSIS
get_sentiments("bing")
masterways_tweets_sentiment =  masterways_tweets %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
masterways_tweets_sentiment

## SENTIMENTS BY PERCENTAGE
table(masterways_tweets_sentiment$sentiment)
prop.table(table(masterways_tweets_sentiment$sentiment))*100

## SENTIMENTS GRAPHICALY
masterways_tweets_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_text(aes(label = percent(prop)),  hjust = 0) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Response of people on the MASTERWAYS brand launch",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_bw() +
  coord_flip()
