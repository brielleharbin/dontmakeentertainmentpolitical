
#Pol Comm Narratives of Racial Duty Data Analysis Files
#Author: M. Brielle Harbin 
#Date: February 28, 2023

#reading in r packages used for analysis 

library(xlsx)
library(tidyverse)
library(dplyr)
library(tidytext)
library(quanteda)
library(academictwitteR)
library(writexl)
library(rtweet)
library(wordcloud)
library(readr)
library(stringr)
library(ggplot2)
library(glue)
library(quanteda)

#########################
##SETTING BEARER TOKEN##
########################

bearer_token <- "REMOVED"


###############################
##RETRIEVING SURVIVOR TWEETS##
##############################


#Building query for replies to episode tweets in reply to CBS account only 

survivor_cbs_engagement <- build_query(reply_to = "survivorcbs")


##############
##EPISODE 02##
##############

survivorcbsonly_ep2_tweets <- get_all_tweets(query = survivor_cbs_engagement, 
                                             start_tweets = "2021-09-29T00:00:00Z", # start date 
                                             end_tweets = "2021-10-05T23:59:59Z", # end date
                                             bearer_token = bearer_token, # bearer token to access Twitter API 
                                             file = "episode2_survivorcbsonly", # name of the resulting RDS file 
                                             n = 1000000
)


#exporting Survivor episode 2 tweets to Excel file 

write_xlsx(survivorcbsonly_ep2_tweets,"survivorcbsonly_ep2_tweetreplies.xlsx")

##############
##EPISODE 05##
##############

#replies to CBS survivor account only 

survivorcbsonly_ep5_tweets <- get_all_tweets(query = survivor_cbs_engagement, 
                                             start_tweets = "2021-10-20T00:00:00Z", # start date 
                                             end_tweets = "2021-10-26T23:59:59Z", # end date
                                             bearer_token = bearer_token, # bearer token to access Twitter API 
                                             file = "episode5_survivorcbsonly", # name of the resulting RDS file 
                                             n = 1000000
)

write_xlsx(survivorcbsonly_ep5_tweets,"survivorcbsonly_ep5_tweetreplies.xlsx")


##############
##EPISODE 06##
##############

#replies to CBS survivor account only 


survivorcbsonly_ep6_tweets <- get_all_tweets(query = survivor_cbs_engagement, 
                                             start_tweets = "2021-10-27T00:00:00Z", # start date 
                                             end_tweets = "2021-11-02T23:59:59Z", # end date
                                             bearer_token = bearer_token, # bearer token to access Twitter API 
                                             file = "episode6_survivorcbsonly", # name of the resulting RDS file 
                                             n = 1000000
)

write_xlsx(survivorcbsonly_ep6_tweets,"survivorcbsonly_ep6_tweetreplies.xlsx")

##############
##EPISODE 11##
##############

survivorcbsonly_ep11_tweets <- get_all_tweets(query = survivor_cbs_engagement, 
                                              start_tweets = "2021-12-01T00:00:00Z", # start date 
                                              end_tweets = "2021-12-07T23:59:59Z", # end date
                                              bearer_token = bearer_token, # bearer token to access Twitter API 
                                              file = "episode11_survivorcbsonly", # name of the resulting RDS file 
                                              n = 1000000
)

write_xlsx(survivorcbsonly_ep11_tweets,"survivorcbsonly_ep11_tweetreplies.xlsx")



#########################################
#IMPORTING CLEANED ANONYMIZED CSV FILES##  
#########################################

#CBS Survivor Account Tweet Replies 

survivorcbsonly_ep2_reactions <- as_tibble(read.csv("Survivor 41 Episode 02 - Survivor 41 CBS Only - Gender Baseline - Anonymized.csv"))

survivorcbsonly_ep5_reactions <- as_tibble(read.csv("Survivor 41 Episode 05 - Survivor 41 CBS Only - Narrative Reactions Only - Anonymized.csv"))

survivorcbsonly_ep6_reactions <- as_tibble(read.csv("Survivor 41 Episode 06 - Survivor CBS Only - Narrative Reactions Only - Anonymized.csv"))

survivorcbsonly_ep11_reactions <- as_tibble(read.csv("Survivor 41 Episode 11 - Survivor 41 CBS Only - Narrative Reactions Only - Anonymized.csv"))



#################################
#TIDYING TWITTER REACTION FILES##    
#################################

#Episode 2

survivorcbsonlyep2Tokens <-survivorcbsonly_ep2_reactions %>% 
  unnest_tokens(word, text) %>% 
  count(episode, word, sort = TRUE) %>% 
  ungroup()

survivorcbsonlyep2Tokens

totalsurvivorcbsonlygep2Tokens <- survivorcbsonlyep2Tokens %>% 
  group_by(episode) %>% 
  summarize(total = sum(n))

totalsurvivorcbsonlygep2Tokens

survivorcbsonlyep2Tokens <- left_join(survivorcbsonlyep2Tokens, totalsurvivorcbsonlygep2Tokens)

survivorcbsonlyep2Tokens 

#Episode 5

survivorcbsonlyep5Tokens <-survivorcbsonly_ep5_reactions %>% 
  unnest_tokens(word, text) %>% 
  count(episode, word, sort = TRUE) %>% 
  ungroup()

survivorcbsonlyep5Tokens

totalsurvivorcbsonlygep5Tokens <- survivorcbsonlyep5Tokens %>% 
  group_by(episode) %>% 
  summarize(total = sum(n))

totalsurvivorcbsonlygep5Tokens

survivorcbsonlyep5Tokens <- left_join(survivorcbsonlyep5Tokens, totalsurvivorcbsonlygep5Tokens)

survivorcbsonlyep5Tokens 


#Episode 6

survivorcbsonlyep6Tokens <-survivorcbsonly_ep6_reactions %>% 
  unnest_tokens(word, text) %>% 
  count(episode, word, sort = TRUE) %>% 
  ungroup()

survivorcbsonlyep6Tokens

totalsurvivorcbsonlygep6Tokens <- survivorcbsonlyep6Tokens %>% 
  group_by(episode) %>% 
  summarize(total = sum(n))

totalsurvivorcbsonlygep6Tokens

survivorcbsonlyep6Tokens <- left_join(survivorcbsonlyep6Tokens, totalsurvivorcbsonlygep6Tokens)

survivorcbsonlyep6Tokens 

#Episode 11


survivorcbsonlyep11Tokens <-survivorcbsonly_ep11_reactions %>% 
  unnest_tokens(word, text) %>% 
  count(episode, word, sort = TRUE) %>% 
  ungroup()

survivorcbsonlyep11Tokens


totalsurvivorcbsonlygep11Tokens <- survivorcbsonlyep11Tokens %>% 
  group_by(episode) %>% 
  summarize(total = sum(n))

totalsurvivorcbsonlygep11Tokens

survivorcbsonlyep11Tokens <- left_join(survivorcbsonlyep11Tokens, totalsurvivorcbsonlygep11Tokens)

totalsurvivorcbsonlygep11Tokens

#####################################################################################
#CREATING CUSTOM STOP WORDS FOR SURVIVOR RELEVANT STOP WORKS FROM TIBBLE DATA FRAME## 
#####################################################################################

custom_stop_words <- bind_rows(data_frame(word = c("show", "tribe", "vote", "tiff", "challenge", "council", "advantage", "merge", "survivor", "survdiversity", "survivorcbs", "survivor41", "https", "t.co", "rt")),
                               stop_words)



###############################
#CONDUCTING SENTIMENT ANALYSIS#
###############################


#CBS Survivor Account Tweet Replies 

#Episode 2 Analysis 

survivorcbsonlyep2_nrc <- survivorcbsonlyep2Tokens  %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words)  %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)  # made data wide rather than narrow

survivorcbsonlyep2_nrc

#Episode 5 Analysis 

survivorcbsonlyep5_nrc <- survivorcbsonlyep5Tokens  %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words)  %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)  # made data wide rather than narrow

survivorcbsonlyep5_nrc

#Episode 6 Analysis 

survivorcbsonlyep6_nrc <- survivorcbsonlyep6Tokens  %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words)  %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)  # made data wide rather than narrow

survivorcbsonlyep6_nrc

#Episode 11 Analysis 

survivorcbsonlyep11_nrc <- survivorcbsonlyep11Tokens  %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  anti_join(custom_stop_words)  %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)  # made data wide rather than narrow

survivorcbsonlyep11_nrc


