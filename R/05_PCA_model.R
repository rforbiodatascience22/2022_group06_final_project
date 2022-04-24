#plot of correlation between diet and BMI
#Is there a correlation between fat intake and BMI?
data <- read_csv("data/03_dat_aug.csv")

#ggplot(data, mapping = aes(BMI,Dfi)) + 
#  geom_point()
data <- data %>% 
  mutate(Dfi_class_words = case_when(Dfi_class == 1 ~ "Low fat intake",
                                     Dfi_class == 2 ~ "Medium fat intake",
                                     Dfi_class == 3 ~ "High fat intake"))
ggplot(data, mapping = aes(BMI,
                           color = Dfi_class_words)) + 
  geom_boxplot()