library(tidyverse)
library(dplyr)
library(zoo)
library(smooth)
library(Mcomp)
library(ggplot2)


##########################
##### PREPARE TABLES #####
##########################


# table with ichimoku indicators
ichimoku <- function(data_table, signal, base) {
  data %>% 
    mutate(
      tenkan_max1 = rollmaxr(as.numeric(open), signal, align = "right", 
                             fill = NA),
      tenkan_min1 = -rollmaxr(-as.numeric(open), signal, align = "right", 
                              fill = NA),
      kijun_max1 = rollmaxr(as.numeric(open), base, align = "right", 
                            fill = NA),
      kijun_min1 = -rollmaxr(-as.numeric(open), base, align = "right", 
                             fill = NA)
    ) %>% 
    mutate(tenkan=(tenkan_max1+tenkan_min1)/2,
           kijun = (kijun_max1+kijun_min1)/2) %>%
    mutate(diff = tenkan-kijun) %>% 
    select(start:volume, tenkan, kijun, diff) %>% 
    na.omit()
}




##########################
##### BACKTESTING ########
##########################


# returns only pl
#parameters: 

#signal table: table with columns start (date), open (open price), signal (1 is buy, -1 is sell)
#fee: fee paid
#start_date, end_date : which timeframe we want to analyse pl in

pl <- function(signal_table, fee, start_date = min(signal_table$start), end_date = max(signal_table$start)) {
  signal_table %>% 
    filter(start >= start_date & start <= end_date) -> signal_table
  signal_table <- cbind(signal_table, pl = 1)
  
  for (i in 2:nrow(signal_table)) {
    if (signal_table$signal[i-1] > 0 ) {
      if (sign(signal_table$signal[i]) == sign(signal_table$signal[i-1])) { # substract fee if trade is conducted
        signal_table$pl[i] = (signal_table$open[i]/signal_table$open[i-1])*signal_table$pl[i-1]
      } else {
        signal_table$pl[i] = (signal_table$open[i]/signal_table$open[i-1]-fee)*signal_table$pl[i-1]
      }
    } else {
      if (sign(signal_table$signal[i]) == sign(signal_table$signal[i-1])) {
        signal_table$pl[i] = (signal_table$open[i-1]/signal_table$open[i])*signal_table$pl[i-1]
      } else {
        signal_table$pl[i] = (signal_table$open[i-1]/signal_table$open[i]-fee)*signal_table$pl[i-1]
      }
    }
  }
  signal_table$pl[nrow(signal_table)]
}




# VISUALISING


# returns the ichimoku table with signals, positions, pl
ichimoku_performance <- function(data_table, signal, base, fee) {
  
  ichimoku_test <- ichimoku(data_table, signal, base)
  
  #signal column
  ichimoku_test <- cbind(ichimoku_test, signal = 0)
  position <- 1 #we start long
  for (i in 1:nrow(ichimoku_test)) {
    
    if (ichimoku_test$diff[i] > 0 & position == -1) {
      ichimoku_test$signal[i] = 1
      position <- 1
    } else if (ichimoku_test$diff[i] < 0 & position == 1) {
      ichimoku_test$signal[i] = -1
      position <- -1
    } else {
      ichimoku_test$signal[i] = ifelse(ichimoku_test$diff[i] != 0, ichimoku_test$signal[i-1], position)
    }
  }
  
  #pl column
  ichimoku_test <- cbind(ichimoku_test, pl = 1)
  for (i in 2:nrow(ichimoku_test)) {
    if (ichimoku_test$signal[i-1] > 0 ) {
      if (sign(ichimoku_test$signal[i]) == sign(ichimoku_test$signal[i-1])) { # substract fee if trade is conducted
        ichimoku_test$pl[i] = (ichimoku_test$open[i]/ichimoku_test$open[i-1])*ichimoku_test$pl[i-1]
      } else {
        ichimoku_test$pl[i] = (ichimoku_test$open[i]/ichimoku_test$open[i-1]-fee)*ichimoku_test$pl[i-1]
      }
    } else {
      if (sign(ichimoku_test$signal[i]) == sign(ichimoku_test$signal[i-1])) {
        ichimoku_test$pl[i] = (ichimoku_test$open[i-1]/ichimoku_test$open[i])*ichimoku_test$pl[i-1]
      } else {
        ichimoku_test$pl[i] = (ichimoku_test$open[i-1]/ichimoku_test$open[i]-fee)*ichimoku_test$pl[i-1]
      }
    }
  }
  
  ichimoku_test %>% select(start, open, signal, pl)
}

