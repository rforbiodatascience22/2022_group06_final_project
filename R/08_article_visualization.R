library(tidyverse)
library(ggpubr)
# Re-creation of plot from article ----------------------------------------
# Load augmented data
data <- read_csv(file = "data/03_dat_aug.csv")

#Recreating plot from article
plt <- ggplot(data, mapping = aes(x = Group_names,
                           y = mtDNA,
                           stratification = Group_names, 
                           fill  = Group_names)) + 
  
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  
  theme_classic() +
  theme(legend.position = "right") + 
  labs(title = "Boxplot of relative mtDNA copy number of controls and PCa cases", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in Han chinese with prostate cancer and healthy controls.") +
  theme(plot.title = element_text(size = 10), 
        plot.caption = element_text(size = 6),
        plot.caption.position = "plot")

plt + ggpubr::stat_compare_means(method = "t.test")


    


