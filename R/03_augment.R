#Load data
data <- read_csv(file = "data/02_dat_clean.csv")

#Create artificial split, remove patient info
patient_info <- data %>% 
  select(Sample, Age, dietary, Smoking_history, Family_hist, BMI)
  
medical_info <- data %>% 
  select(!c(Age, dietary, Smoking_history, Family_hist, BMI))

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
  mutate(dietary_class = case_when(dietary < 20 ~ 1,
                                   20 <= dietary & dietary < 30 ~ 2,
                                   30 <= dietary ~ 3))

#Join patient info and medical info based in Sample
data_augmented <- patient_info %>% 
  full_join(medical_info,
            by = "Sample")

#Write the data file
write_csv("data/03_dat_aug.csv")