library(tidyverse)

filtered_data <- read_csv("data/01_dat_load.csv") %>% 
  filter(PCRsuccess == 1)
  # add_row(Sample = "CM4033")

# is_duplicated <- filtered_data %>% 
#   group_by(Sample) %>% 
#   filter(n() > 1) %>% 
#   nrow()
# if_else(is_duplicated > 0 ~ print("ERROR: Duplicates in dataset"))

filtered_data %>% 
  write_csv("data/02_dat_clean.csv")
