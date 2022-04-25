library(tidyverse)

#load dataset and filter out samples with non-successful PCR experiments
filtered_data <- read_csv("data/01_dat_load.csv") %>% 
  filter(PCRsuccess == 1) 

#check for duplicates
is_duplicated <- filtered_data %>% 
  group_by(Sample) %>% 
  filter(n() != 1)

#print error if sample duplicates are present in the data frame
if (nrow(is_duplicated) > 0) {
  printstr = nrow(is_duplicated)
  print(paste("Number of duplicates in the dataset:", printstr))
}
#nrow(is_duplicated)
filtered_data %>% 
  write_csv("data/02_dat_clean.csv")
