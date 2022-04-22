library(tidyverse)

read_csv("data/01_dat_load.csv") %>% 
  filter(PCRsuccess == 1) %>% 
  mutate()
  write_csv("data/02_dat_clean.csv")
