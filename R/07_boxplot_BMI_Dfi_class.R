library(tidyverse)
library(ggsignif)

# Correlation between fat intake and BMI ----------------------------------
# Load augmented data
data <- read_csv("data/03_dat_aug.csv")

#Boxplot
ggplot(data, mapping = aes(x = dfi_class,
                           y = bmi,
                           fill = dfi_class)) + 
  geom_signif(comparisons = list(c("high fat",
                                   "medium fat"),
                                 c("medium fat",
                                   "low fat")), 
              map_signif_level = TRUE,
              size = 0.4) +
  geom_signif(comparisons = list(c("high fat",
                                   "low fat")),
              map_signif_level = TRUE,
              y_position = 39) +
  geom_boxplot() +
  scale_x_discrete(limits = c("low fat", 
                              "medium fat", 
                              "high fat"))

ggsave(filename = "results/boxplot_bmi_dfi.png",
       width = 10,
       height = 7)
