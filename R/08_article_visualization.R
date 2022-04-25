library(tidyverse)

# Re-creation of plot from article ----------------------------------------
# Load augmented data
data <- read_csv(file = "data/03_dat_aug.csv")

#Recreating plot from article
ggplot(data,
       mapping = aes(x = Group,
                     y = mtDNA,
                     stratification = Group, 
                     fill  = Group)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  theme_classic() +
  theme(legend.position = "right") +
  labs(title = "Plot", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in Han chinese with prostate cancer and healthy controls.") 
      
    
  


