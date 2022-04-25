library(tidyverse)
library(readxl)

# Load data sheet
# Since the data structure of the PSA level attribute column is 
# set to "custom" in the data/_raw/Data.xlsx and not "general"
# the data is loaded as a date and time and not a number.


# Fix wrong guesses -------------------------------------------------------
#readr takes a wrong guess when guessing the type of data for column "PSA level
# (ng/ml)". Instead we get column name and uses an if-else statement
nms <- names(read_excel("data/_raw/Data.xlsx",
                        sheet = "Data Set",
                        n_max = 0))
cnames <- ifelse(str_detect(nms, "^PSA"), "numeric", "guess")


# Load data set -----------------------------------------------------------
read_excel("data/_raw/Data.xlsx",
           sheet = "Data Set",
           col_types = cnames,
           .name_repair = "universal") %>%
  rename(Age = Age..yr.,
         TNM = TNM.stage,
         AJCC = AJCC.stage,
         PSA = PSA.level..ng.ml.,
         Gleason = Gleason.score,
         Dfi = Daily.fat.dietary.intake....,
         Smoking = Smoking.history,
         PCaHist = Family.history.of.PCa,
         BMI = BMI..kg.m2.,
         mtDNA = mtDNA.copy.number) %>% 
  write_csv("data/01_dat_load.csv")

# Load legend data --------------------------------------------------------
read_excel("data/_raw/Data.xlsx",
           sheet = "Column Legend") %>%
  write_csv("data/01_legend_load.csv")
