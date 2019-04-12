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

