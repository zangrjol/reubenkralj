library(tidyverse)
load("ichimoku/data.rda")


# function to generate a new table with ichimoku columns

ichimoku <- function(signal, base) {
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

#another column which tells when to buy/sell
#1 is buy, -1 is sell


ichimoku_signal <- function(signal, base, fee) {
  
  ichimoku_test <- ichimoku(signal, base)
  
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
      ichimoku_test$signal[i] = ifelse(i > 0, ichimoku_test$signal[i-1], position)
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
  
  ichimoku_test
}
position <- 1 #we start long
for (i in 1:nrow(ichimoku_test)) {

  if (ichimoku_test$diff[i] > 0 & position == -1) {
    ichimoku_test$signal[i] = 1
    position <- 1
  } else if (ichimoku_test$diff[i] < 0 & position == 1) {
    ichimoku_test$signal[i] = -1
    position <- -1
  } else {
    ichimoku_test$signal[i] = ifelse(i > 0, ichimoku_test$signal[i-1], position)
  }
}

ichimoku_test <- cbind(ichimoku_test, pl = 1)
fee <- 0.00025
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


test <- ichimoku_signal(15,16,0.00025)
View(test)
