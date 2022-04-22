diabetes_metric <- diabetes_metric %>% 
  mutate(BMI_class = case_when(BMI < 18.5 ~ "underweight",
                               18.5 <= BMI & BMI < 25 ~ "normal weight",
                               25 <= BMI & BMI < 30 ~ "overweight",
                               30 <= BMI & BMI < 35 ~ "obese",
                               35 <= BMI & BMI < 40 ~ "severe obesity",
                               40 <= BMI & BMI < 45 ~ "morbid obesity",
                               45 <= BMI ~ "super obese"))

diabetes_metric <- diabetes_metric %>%
  mutate(BMI_class = factor(BMI_class,
                            levels =  c("underweight", "normal weight",
                                        "overweight", "obese",
                                        "severe obesity", "morbid obesity",
                                        "super obese")))