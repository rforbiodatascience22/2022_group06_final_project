# Load data sheet
# Since the data structure of the PSA level attribute column is 
# set to "custom" in the data/_raw/Data.xlsx and not "general"
# the data is loaded as a date and time and not a number.


# Fix wrong guesses -------------------------------------------------------
# readr takes a wrong guess when guessing the type of data for column "PSA level
# (ng/ml)". Instead we get column name and uses an if-else statement.
nms <- readxl::read_excel(path = "data/_raw/Data.xlsx",
                          sheet = "Data Set",
                          n_max = 0) %>% 
  names()


cnames <- ifelse(str_detect(string = nms, pattern = "^PSA"), "numeric", "guess")


# Load data set -----------------------------------------------------------
# In addition to loading the data naming is changed to snake_case
# Uses 'cnames' to decide columntypes of excel-file.
read_excel(path = "data/_raw/Data.xlsx",
           sheet = "Data Set",
           col_types = cnames,
           .name_repair = "universal") %>%
  rename(sample = Sample,
         group = Group,
         pcr_success = PCRsuccess,
         age = Age..yr.,
         tnm = TNM.stage,
         ajcc = AJCC.stage,
         psa = PSA.level..ng.ml.,
         gleason = Gleason.score,
         dfi = Daily.fat.dietary.intake....,
         smoking = Smoking.history,
         cancer_hist = Family.history.of.PCa,
         bmi = BMI..kg.m2.,
         mtdna = mtDNA.copy.number) %>% 
  write_csv(path = "data/01_dat_load.csv")

# Load legend data --------------------------------------------------------
read_excel(path = "data/_raw/Data.xlsx",
           sheet = "Column Legend") %>%
  write_csv(path = "data/01_legend_load.csv")

