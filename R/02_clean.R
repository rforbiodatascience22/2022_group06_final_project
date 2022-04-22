library(tidyverse)

filtered_data <- read_csv("data/01_dat_load.csv") %>% 
  filter(PCRsuccess == 1) 

is_duplicated <- filtered_data %>% 
  group_by(Sample) %>% 
  filter(n() != 1) %>% 
  ungroup() %>% 
  pull()
print(str("Are there any duplications in the dataset?", is_duplicated))


filtered_data %>% 
  write_csv("data/02_dat_clean.csv")
