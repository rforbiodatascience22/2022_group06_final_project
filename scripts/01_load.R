library(tidyverse)
library(readxl)

data <- read_excel("data/_raw/Data.xlsx",
                   sheet = 2) %>% 
  write_csv("data/raw_data.csv")
info <- read_excel("data/_raw/Data.xlsx",
                   sheet = 1) %>%
  write_csv("data/info.csv")

