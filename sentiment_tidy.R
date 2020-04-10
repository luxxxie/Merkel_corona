library(tidytext)
data <- read_csv("data.csv")
data_de <- data %>% 
  filter(language == "DE") %>% 
  distinct()