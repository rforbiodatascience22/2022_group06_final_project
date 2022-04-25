#Load data
data <- read_csv(file = "data/02_dat_clean.csv")

#Create artificial split, remove patient info
patient_info <- data %>% 
  select(Sample, Age, Dfi, Smoking, PCaHist, BMI)
  
medical_info <- data %>% 
  select(!c(Age, Dfi, Smoking, PCaHist, BMI))

#Add BMI_class and convert into factors
patient_info <- patient_info %>% 
  mutate(BMI_class = case_when(BMI < 18.5 ~ "underweight",
                               18.5 <= BMI & BMI < 25 ~ "normal weight",
                               25 <= BMI & BMI < 30 ~ "overweight",
                               30 <= BMI ~ "obese"),
         BMI_class = factor(BMI_class,
                            levels =  c("underweight", "normal weight",
                                        "overweight", "obese")))

#Add dietary_class
patient_info <- patient_info %>% 
  mutate(Dfi_class = case_when(Dfi < 20 ~ "low fat",
                                   20 <= Dfi & Dfi < 30 ~ "medium fat",
                                   30 <= Dfi ~ "high fat"))

#Add variables derived from TNM 
#Data points that do not follow the standard naming convention 
#are left as "NA", convention taken from www.cancerresearchuk.org
medical_info <- medical_info %>% 
  mutate(Tumor = case_when(str_detect(TNM,"T0.+") ~ 0,
                           str_detect(TNM,"T1.+") ~ 1,
                           str_detect(TNM,"T2.+") ~ 2,
                           str_detect(TNM,"T3.+") ~ 3,
                           str_detect(TNM,"T4N.+") ~ 4,
                           str_detect(TNM,"T4a.+") ~ 5,
                           str_detect(TNM,"T4b.+") ~ 6),
         LymphNodes = case_when(str_detect(TNM,".+N0.+") ~ 0,
                                str_detect(TNM,".+N1.+") ~ 1,
                                str_detect(TNM,".+N2.+") ~ 2,
                                str_detect(TNM,".+N3.+") ~ 3),
         Metastasis = case_when(str_detect(TNM,".+M0") ~ 0,
                                str_detect(TNM,".+M1a") ~ 1,
                                str_detect(TNM,".+M1b") ~ 2,
                                str_detect(TNM,".+M1c") ~ 3,
                                # observing the data revealed two points 
                                # that were entered incorrectly, which
                                # we handle at this point
                                str_detect(TNM,".+Mlb") ~ 2,
                                str_detect(TNM,".+m0") ~ 0)

         )



#Join patient info and medical info based in Sample
data_augmented <- patient_info %>% 
  full_join(medical_info,
            by = "Sample")

#Write the data file
data_augmented %>% 
  write_csv("data/03_dat_aug.csv")
