library(tidyverse)
library(stringi)
# Load in clean data ------------------------------------------------------
data <- read_csv(file = "data/02_dat_clean.csv")


# Make artificial split. Split into patient- and medical info --------------
patient_info <- data %>% 
  select(sample, age, dfi, smoking, pca_hist, bmi)
  
medical_info <- data %>% 
  select(!c(age, dfi, smoking, pca_hist, bmi))


# Add in a bmi classifier -------------------------------------------------
patient_info <- patient_info %>% 
  mutate(bmi_class = case_when(bmi < 18.5 ~ "underweight",
                               18.5 <= bmi & bmi < 25 ~ "normal weight",
                               25 <= bmi & bmi < 30 ~ "overweight",
                               30 <= bmi ~ "obese"),
         bmi_class = factor(bmi_class,
                            levels =  c("underweight", "normal weight",
                                        "overweight", "obese")))


# Add in a dfi classifier -------------------------------------------------
patient_info <- patient_info %>% 
  mutate(dfi_class = case_when(dfi < 20 ~ "low fat",
                                   20 <= dfi & dfi < 30 ~ "medium fat",
                                   30 <= dfi ~ "high fat"))


# Split TNM notation into usable numbers ----------------------------------
#Add variables derived from TNM 
#Standard naming convention taken from www.cancerresearchuk.org
medical_info <- medical_info %>% 
  mutate(tumor = case_when(str_detect(tnm,"T0.+") ~ 0,
                           str_detect(tnm,"T1.+") ~ 1,
                           str_detect(tnm,"T2.+") ~ 2,
                           str_detect(tnm,"T3.+") ~ 3,
                           str_detect(tnm,"T4N.+") ~ 4,
                           str_detect(tnm,"T4a.+") ~ 5,
                           str_detect(tnm,"T4b.+") ~ 6),
         lymph_nodes = stri_match(tnm, regex = "N(\\d)"),
         metastasis = case_when(str_detect(tnm,".+M0") ~ 0,
                                str_detect(tnm,".+M1a") ~ 1,
                                str_detect(tnm,".+M1b") ~ 2,
                                str_detect(tnm,".+M1c") ~ 3,
                                # observing the data revealed two points 
                                # that were entered incorrectly, which
                                # we handle at this point
                                str_detect(tnm,".+Mlb") ~ 2,
                                str_detect(tnm,".+m0") ~ 0))


# Add group names ------------------------------------------------
medical_info <- medical_info %>% 
  mutate(group_names = case_when(group == 1 ~ "pca_cases",
                                 group == 0 ~ "controls"))

# Join patient info and medical info based on Sample ----------------------
data_augmented <- patient_info %>% 
  full_join(medical_info,
            by = "sample")


# Write the augmented data file -----------------------------------------------------
data_augmented %>% 
  write_csv("data/03_dat_aug.csv")
