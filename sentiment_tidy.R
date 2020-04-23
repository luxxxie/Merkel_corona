library(tidyverse)
library(tidytext)
data <- read_csv("data.csv")

more_stopwords <- tibble(word = c("t.co","https"), 
                         lexicon = "twitter")

data_tweets_tidy <- data %>% 
  filter(language == "DE") %>% 
  distinct() %>% 
  unnest_tokens(word, Text) %>%
  anti_join(get_stopwords(language = "de")) %>% 
  anti_join(more_stopwords) %>% 
  mutate(token_stem = wordStem(word, "de"))

data_tweets_tidy %>%
  count(word, token_stem, sort = TRUE)

# load Rauh dictionary (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BKBXWD)
load("Rauh_SentDictionaryGerman.Rdata")

Rauh_sent <- sent.dictionary %>% unnest_tokens(word, feature) %>% tibble()
Rauh_sent$sentiment <- Rauh_sent$sentiment %>%
  as.factor() 
Rauh_sent <- Rauh_sent %>%
  mutate(sentiment_d = case_when(
    sentiment == "-1" ~ "negative",
    sentiment == "1" ~ "positive",
  ))

Rauh_sent

# most frequent words
data_tweets_tidy %>% 
  inner_join(Rauh_sent, by = "word") %>%
  count(sentiment_d, word, sort = TRUE)

## tweetwise sentiment scoring
tweet_sentiment <- data_tweets_tidy %>% 
  inner_join(Rauh_sent) %>% 
  mutate(date_published = as.Date(date_published, "%m/%d/%Y")) %>% 
  filter(date_published < "2020/03/21")

tweet_sentiment_total <- data_tweets_tidy %>% 
  full_join(Rauh_sent) %>% 
  mutate(sentiment_d = case_when(
    sentiment == -1 ~ "negative",
    sentiment == 1 ~ "positive",
    TRUE ~"neutral"
  )) %>%
  mutate(date_published = as.Date(date_published, "%m/%d/%Y")) %>% 
  filter(date_published < "2020/03/21") 

# data %>% 
#   filter(language == "DE") %>% 
#   left_join(tweet_sentiment %>% 
#               group_by(`tweet url`) %>%  # and `Tweet_id`, User_id
#               summarise(score = sum(sentiment))) %>% 
#   replace_na(list(score = 0)) %>% 
#   arrange(desc(score)) 

ggplot(tweet_sentiment, aes(keyword, fill = sentiment_d)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  # facet_grid(.~keyword) +
  labs(x = "hashtag", y = "proportion", fill = "sentiment") +
  scale_fill_manual(values=c("grey25", "grey75"), labels=c("positive", "negative")) +
  theme_bw()

ggsave("twitter_prop.pdf", width = 7, height = 3, dpi = 300)
ggsave("twitter_prop.png", width = 7, height = 3, dpi = 300)


ggplot(tweet_sentiment_total, aes(keyword, fill = sentiment_d)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  # facet_grid(.~keyword) +
  labs(x = "hashtag", y = "proportion", fill = "sentiment") +
  scale_fill_grey() +
  theme_bw()

ggsave("twitter_prop_full.pdf", width = 7, height = 3, dpi = 300)
ggsave("twitter_prop_full.png", width = 7, height = 3, dpi = 300)

# ggplot(tweet_sentiment, aes(sentiment_d, fill = sentiment_d)) +
#   geom_bar(show.legend = FALSE) +
#   facet_grid(.~keyword) +
#   xlab("sentiment") +
#   scale_fill_manual(values=c("grey25", "grey75"), labels=c("positive", "negative")) +
#   theme_bw()
# 
# ggsave("twitter_sentiments.pdf", width = 7, height = 3, dpi = 300)
# ggsave("twitter_sentiments.png", width = 7, height = 3, dpi = 300)

# most emotional words
data_tweets_tidy %>% 
  inner_join(Rauh_sent, by = "word") %>%
  group_by(word) %>% 
  summarise(score = sum(sentiment)) %>% 
  #arrange(desc(score)) %>% 
  arrange(score)


# twitter liwc ------------------------------------------------------------
liwc_words <- readxl::read_xlsx("C:/Users/han/Dropbox/Public/LenaDropboxOul/Merkel_Paper/Merkel_Corona/Sentiment/LIWC_German3.xlsx")
names(liwc_words)[9] <- "v8"
names(liwc_words)[1] <- "word"
names(liwc_words)

### use this below code if you figure out how to match emotion codes with words in different tibble 
# liwc_cats <- read_table2("C:/Users/han/Dropbox/Public/LenaDropboxOul/Merkel_Paper/Merkel_Corona/Sentiment/LIWC_German.txt", 
#                        col_names = c("code", "cat")) %>% 
#   slice(2:65) %>% 
#   mutate_if(is.character,as.factor)
# 
# head(liwc_cats)
# 
# emotion_liwc <- liwc_cats[12:19,] 

### until then... the stupid method
affect <- liwc_words %>% 
  filter_all(any_vars(. == 12)) %>% 
  select(word) %>% 
  mutate(liwc = "affect")

positive <- liwc_words %>% 
  filter_all(any_vars(. == 13 & . == 14)) %>% 
  select(word) %>% 
  mutate(liwc = "positive")

optimism <- liwc_words %>% 
  filter_all(any_vars(. == 15)) %>% 
  select(word) %>% 
  mutate(liwc = "optimism")

negative <- liwc_words %>% 
  filter_all(any_vars(. == 16)) %>% 
  select(word) %>% 
  mutate(liwc = "negative")

anxiety <- liwc_words %>% 
  filter_all(any_vars(. == 17)) %>% 
  select(word) %>% 
  mutate(liwc = "anxiety")

anger <- liwc_words %>% 
  filter_all(any_vars(. == 18)) %>% 
  select(word) %>% 
  mutate(liwc = "anger")

sad <- liwc_words %>% 
  filter_all(any_vars(. == 19)) %>% 
  select(word) %>% 
  mutate(liwc = "sad")

emotion_liwc <- bind_rows(optimism, anxiety, anger, sad)

tweet_liwc <- data_tweets_tidy %>% 
  inner_join(emotion_liwc) %>% 
  mutate(date_published = as.Date(date_published, "%m/%d/%Y")) %>% 
  filter(date_published < "2020/03/21")

tweet_liwc_full <- data_tweets_tidy %>% 
  full_join(emotion_liwc) %>% 
  mutate(liwc = as_factor(liwc)) %>% 
  mutate(date_published = as.Date(date_published, "%m/%d/%Y")) %>% 
  filter(date_published < "2020/03/21")

tweet_liwc_full$liwc <- tweet_liwc_full$liwc %>% 
  fct_explicit_na(na_level = "neutral")

ggplot(tweet_liwc, aes(keyword, fill = liwc)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  # facet_grid(.~keyword) +
  labs(x = "hashtag", y = "proportion", fill = "emotional content") +
  # scale_fill_manual(values=c("grey25", "grey75"), labels=c("positive", "negative")) +
  scale_fill_grey() +
  theme_bw()

ggsave("twitter_prop_liwc.pdf", width = 7, height = 3, dpi = 300)
ggsave("twitter_prop_liwc.png", width = 7, height = 3, dpi = 300)

ggplot(tweet_liwc_full, aes(keyword, fill = liwc)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  # facet_grid(.~keyword) +
  labs(x = "hashtag", y = "proportion", fill = "emotional content") +
  # scale_fill_manual(values=c("grey25", "grey75"), labels=c("positive", "negative")) +
  scale_fill_grey() +
  theme_bw()

ggsave("twitter_prop_liwc_full.pdf", width = 7, height = 3, dpi = 300)
ggsave("twitter_prop_liwc_full.png", width = 7, height = 3, dpi = 300)

# newspapers --------------------------------------------------------------
data_corona_welt <- readRDS("C:/Users/han/Dropbox/Public/LenaDropboxOul/Merkel_Paper/Merkel_Corona/Data/data_corona_welt.RDS")
data_corona_tag <- readRDS("C:/Users/han/Dropbox/Public/LenaDropboxOul/Merkel_Paper/Merkel_Corona/Data/data_corona_tagesspiegel.RDS")
data_corona_sued <- readRDS("C:/Users/han/Dropbox/Public/LenaDropboxOul/Merkel_Paper/Merkel_Corona/Data/data_corona_sueddeutsche.RDS")

#data_corona_sued$pub <- sub(" \\(inkl. Regionalausgaben\\)", replacement = "", data_corona_sued$pub)

data_corona_sued$pub <- sub("Süddeutsche Zeitung.*", replacement = "Süddeutsche Zeitung", data_corona_sued$pub)

data_news <- bind_rows(data_corona_sued, data_corona_welt, data_corona_tag)

rm(data_corona_welt, data_corona_tag, data_corona_sued)

tidy_news <- data_news %>% 
  distinct() %>% 
  unnest_tokens(word, body) %>%
  anti_join(get_stopwords(language = "de")) %>% 
  anti_join(more_stopwords)
  
# most frequent words
tidy_news %>% 
  inner_join(Rauh_sent, by = "word") %>%
  count(sentiment_d, word, sort = TRUE)

## newspaperwise sentiment scoring WITH SENTENCES THAT CONTAIN MERKEL
tidy_news_sentences <- data_news %>% 
  distinct() %>% 
  filter(str_detect(body, 'ansprache|rede')) %>% 
  unnest_tokens(sentence, body, token = "sentences") %>% 
  filter(str_detect(sentence, 'merkel')) %>% 
  unnest_tokens(word, sentence) 

# tidy_news_sentences <- data_news %>% 
#   distinct() %>% 
#   unnest_tokens(sentence, body, token = "sentences") %>% 
#   filter(str_detect(sentence, 'merkel')) %>% 
#   unnest_tokens(word, sentence) 

news_sentiment_sentences <- tidy_news_sentences %>% 
  inner_join(Rauh_sent) %>% 
  mutate(date_published = as.Date(date, "%Y-%m-%d")) %>% 
  filter(date_published < "2020-03-21" & date_published > "2020-03-17") 

news_sentiment_sentences_full <- tidy_news_sentences %>% 
  full_join(Rauh_sent) %>% 
  mutate(sentiment_d = case_when(
    sentiment == -1 ~ "negative",
    sentiment == 1 ~ "positive",
    TRUE ~"neutral"
  )) %>%
  mutate(date_published = as.Date(date, "%Y-%m-%d")) %>% 
  filter(date_published < "2020-03-21" & date_published > "2020-03-17") 

news_sentiment_sentences_full$sentiment_d

ggplot(news_sentiment_sentences_full, aes(pub, fill = sentiment_d)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  #facet_wrap(.~pub, scales = "free") +
  labs(fill = "sentiment", x = "newspaper", y = "proportion") +
  scale_fill_manual(values=c("grey25", "grey75", 'black'), labels=c("positive", "negative", "neutral")) +
  theme_bw()

ggsave("news_prop_full.pdf", width = 8, height = 3, dpi = 300)
ggsave("news_prop_full.png", width = 8, height = 3, dpi = 300)

ggplot(news_sentiment_sentences, aes(pub, fill = sentiment_d)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  # facet_wrap(.~, scales = "free") +
  labs(fill = "sentiment", x = "newspaper", y = "proportion") +
  scale_fill_manual(values=c("grey25", "grey75", 'black'), labels=c("positive", "negative", "neutral")) +
  theme_bw()

ggsave("news_prop.pdf", width = 6, height = 3, dpi = 300)
ggsave("news_prop.png", width = 6, height = 3, dpi = 300)

ggplot(news_sentiment_sentences, aes(sentiment_d, fill = sentiment_d)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(.~pub, scales = "free") +
  xlab("sentiment") +
  ylab("word count") +
  scale_fill_manual(values=c("grey25", "grey75"), labels=c("positive", "negative")) +
  theme_bw()

ggsave("news_sentiments.pdf", width = 8, height = 3, dpi = 300)
ggsave("news_sentiments.png", width = 8, height = 3, dpi = 300)


# newspaper liwc ----------------------------------------------------------
news_liwc_sentences <- tidy_news_sentences %>% 
  inner_join(emotion_liwc) %>% 
  mutate(date_published = as.Date(date, "%Y-%m-%d")) %>% 
  filter(date_published < "2020-03-21" & date_published > "2020-03-17") 
# 
# ggplot(news_sentiment_sentences_full, aes(pub, fill = sentiment_d)) +
#   geom_bar(show.legend = TRUE, position = "fill") +
#   #facet_wrap(.~pub, scales = "free") +
#   labs(fill = "sentiment", x = "newspaper", y = "proportion") +
#   scale_fill_manual(values=c("grey25", "grey75", 'black'), labels=c("positive", "negative", "neutral")) +
#   theme_bw()
# 
# ggsave("news_prop_full.pdf", width = 8, height = 3, dpi = 300)
# ggsave("news_prop_full.png", width = 8, height = 3, dpi = 300)

ggplot(news_liwc_sentences, aes(pub, fill = liwc)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  # facet_wrap(.~, scales = "free") +
  labs(fill = "emotional content", x = "newspaper", y = "proportion") +
  scale_fill_grey() + 
  theme_bw()

ggsave("news_prop_liwc.pdf", width = 6, height = 3, dpi = 300)
ggsave("news_prop_liwc.png", width = 6, height = 3, dpi = 300)

news_liwc_sentences_full <- tidy_news_sentences %>% 
  full_join(emotion_liwc) %>% 
  mutate(liwc = as_factor(liwc)) %>% 
  mutate(date_published = as.Date(date, "%Y-%m-%d")) %>% 
  filter(date_published < "2020-03-21" & date_published > "2020-03-17") 

news_liwc_sentences_full$liwc <- news_liwc_sentences_full$liwc %>% 
  fct_explicit_na(na_level = "neutral")

ggplot(news_liwc_sentences_full, aes(pub, fill = liwc)) +
  geom_bar(show.legend = TRUE, position = "fill") +
  #facet_wrap(.~pub, scales = "free") +
  labs(fill = "emotional content", x = "newspaper", y = "proportion") +
  scale_fill_grey() +
  theme_bw()

ggsave("news_prop_liwc_full.pdf", width = 8, height = 3, dpi = 300)
ggsave("news_prop_liwc_full.png", width = 8, height = 3, dpi = 300)

