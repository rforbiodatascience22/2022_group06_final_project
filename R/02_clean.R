library(tidyverse)


# Clean out unsuccessful data ---------------------------------------------
filtered_data <- read_csv(path = "data/01_dat_load.csv",
                          show_col_types = FALSE) %>% 
  filter(pcr_success == 1) 


# Check for duplicates ----------------------------------------------------
is_duplicated <- filtered_data %>% 
  group_by(sample) %>% 
  filter(n() != 1)

# Print error if sample duplicates are present in the data frame.
if (nrow(is_duplicated) > 0) {
  printstr = nrow(is_duplicated)
  print(str_c("Number of duplicates in the dataset: ", printstr))
}


# Export filtered data. ---------------------------------------------------
filtered_data %>% 
  write_csv(path = "data/02_dat_clean.csv")

