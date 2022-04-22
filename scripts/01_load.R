library(tidyverse)

# Load data sheet
# Since the data structure of the PSA level attribute column is 
# set to "custom" in the data/_raw/Data.xlsx and not "general"
# the data is loaded as a date and time and not a 
data_raw <- read_excel("data/_raw/Data.xlsx",
                   sheet = 2) %>% 
  write_csv("data/01_dat_load.csv")

# Load legend sheet
legend <- read_excel("data/_raw/Data.xlsx",
                   sheet = 1) %>%
  write_csv("data/01_legend_load.csv")



