library(tidyverse)
data <- read_csv2("ichimoku/data1h.csv", col_names = TRUE,)
data <- data %>%
  mutate(start = as.POSIXct(as.character(start), format = "%d/%m/%Y %H:%M"))

save(data, file = "ichimoku/data.rda")

