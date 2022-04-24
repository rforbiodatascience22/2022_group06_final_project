library(tidyverse)
#plot of correlation between diet and BMI
#Is there a correlation between fat intake and BMI?
data <- read_csv("data/03_dat_aug.csv")

ggplot(data, mapping = aes(BMI,Dfi)) + 
  geom_point()

ggplot(data, mapping = aes(x = BMI,
                           y = Dfi_class,
                           fill = Dfi_class)) + 
  geom_boxplot() +
  scale_y_discrete(limits = c("low fat", "medium fat", "high fat"))

