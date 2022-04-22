library(tidyverse)

read_csv("data/01_dat_load.csv") %>% 
  filter(PCRsuccess == 1) %>% 
  write_csv("data/02_dat_clean.csv")
