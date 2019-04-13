library(tidyverse)
library(dplyr)
library(zoo)
library(smooth)
library(Mcomp)
library(ggplot2)
load("ichimoku/data.rda")


# PREPARE TABLE: filter out to only when the position changes
signal_df <- ichimoku(data, 9, 27) %>% 
  filter(diff != 0) %>%
  mutate(signal = ifelse(diff > 0, 1, -1)) %>% 
  select(-diff) %>% 
  mutate(signal2 = rollmeanr(signal, 2, fill = 0)) %>%
  filter(signal2 ==0) %>% 
  select(start, open, signal)






