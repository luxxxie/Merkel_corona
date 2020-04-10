install.packages("remotes")
remotes::install_github("lingeringcode/tweetscoresmod")
library(readr)
library(tidyverse)
library(rtweet)
library(tweetscoresmod)
load("~/my_oauth")
data <- read_csv("data.csv")
View(data)
userid_str_de <- data %>% 
  filter(language == "DE") %>% 
  distinct() %>% 
  select(User_id) %>%
  as.list()

## get twitter ideologies according to barbera
my_oath

followers <- getFriends(user_id = userid_str_de,
                        oauth = my_oath)
