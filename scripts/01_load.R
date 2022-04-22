library(tidyverse)
library(readxl)

#Load data sheet
data_raw <- read_excel("data/_raw/Data.xlsx",
                   sheet = 2) %>% 
  write_csv("data/01_dat_load.csv")

#Load legend sheet
legend <- read_excel("data/_raw/Data.xlsx",
                   sheet = 1) %>%
  write_csv("data/01_legend_load.csv")



