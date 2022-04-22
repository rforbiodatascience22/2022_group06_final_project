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
  mutate(Dfi_class = case_when(Dfi < 20 ~ 1,
                                   20 <= Dfi & Dfi < 30 ~ 2,
                                   30 <= Dfi ~ 3))

#Add variables derived from TNM 
medical_info <- medical_info %>% 
  #select(TNM) %>% 
  mutate(Tumor = case_when(str_detect(TNM,"T4.+") ~ 4,
                           #str_detect(TNM,"T0.") ~ 0,
                           #select(TNM, matches("T1*")) ~ 1,
                           #select(TNM, matches("T2*")) ~ 2,
                           #select(TNM, matches("T3*")) ~ 3,
                           #select(TNM, matches("TX.")) ~ "NA",)#,
         #LymphNodes = case_when(),
         #Metastasis = case_when()
         ))



#Join patient info and medical info based in Sample
data_augmented <- patient_info %>% 
  full_join(medical_info,
            by = "Sample")

#Write the data file
data_augmented %>% 
  write_csv("data/03_dat_aug.csv")
