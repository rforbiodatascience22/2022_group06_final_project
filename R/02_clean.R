library(tidyverse)

#load dataset and filter out samples with non-successful PCR experiments
filtered_data <- read_csv("data/01_dat_load.csv") %>% 
  filter(PCRsuccess == 1) 

#check for duplicates
is_duplicated <- filtered_data %>% 
  group_by(Sample) %>% 
  filter(n() != 1)

#print error if sample duplicates are present in the data frame
if (dim(is_dublicated)[0] > 0) {
  print(str("Number of duplicates in the dataset:", dim(is_duplicated)[0]))
}

filtered_data %>% 
  write_csv("data/02_dat_clean.csv")
