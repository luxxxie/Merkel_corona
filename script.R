install.packages("remotes")
remotes::install_github("lingeringcode/tweetscoresmod")
library(readr)
library(tidyverse)
library(tweetscoresmod)
data <- read_csv("data.csv")
View(data)
userid_str_de <- data %>% 
  filter(language == "DE") %>% 
  distinct() %>% 
  select(User_id) %>%
  as.list()


