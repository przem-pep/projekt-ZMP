library(tidyverse)
library(forecast)

dane <- read_csv("data/dane.csv")

# Standaryzacja

dane_scale <- dane %>%
  select(-date) %>%
  sapply(scale) %>%
  as_tibble()

dane_scale$miernik <- rowSums(dane_scale)

dane_scale$date <- dane$date

train <- slice_head(dane_scale, n = 108)
test <- slice_tail(dane_scale, n = 24)

