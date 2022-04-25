library(tidyverse)


# Correlation between fat intake and BMI ----------------------------------
# Load augmented data
data <- read_csv("data/03_dat_aug.csv")

#Scatterplot
ggplot(data, mapping = aes(BMI,Dfi)) + 
  geom_point()

#Boxplot
ggplot(data, mapping = aes(x = BMI,
                           y = Dfi_class,
                           fill = Dfi_class)) + 
  geom_boxplot() +
  scale_y_discrete(limits = c("low fat", "medium fat", "high fat"))

