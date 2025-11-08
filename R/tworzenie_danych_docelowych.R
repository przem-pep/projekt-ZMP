library(tidyverse)
library(readxl)

dane <- read_excel("data/dane-surowe.xlsx")

dane <- dane %>%
  mutate(date = ym(date)) %>%
  na.omit()

write_csv(dane, "data/dane.csv")
