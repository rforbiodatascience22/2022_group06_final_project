library(tidyverse)
library(ggpubr)


# Boxplot for PSA-levels --------------------------------------------------
# Load augmented data
data <- read_csv(file = "data/03_dat_aug.csv")

#Boxplot
boxplot_psa <- ggplot(data, mapping = aes(x = group_names,
                                   y = psa, 
                                   fill  = group_names)) + 
    stat_boxplot(geom = "errorbar",
               width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  geom_signif(comparisons = list(c("Controls",
                                   "PCa cases")), 
              map_signif_level=TRUE,
              size = 0.4) +
  theme(legend.position = "right") + 
  labs(title = "Boxplot of PSA of controls and PCa cases", 
       y = "PSA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number
                  in Han chinese with prostate cancer and healthy controls.") +
  theme(text = element_text(size = 16),
        plot.caption.position = "plot")  

#Add statistical test
boxplot_psa <- boxplot_psa + ggpubr::stat_compare_means(method = "t.test", size = 10)

#Save figure
ggsave(filename = "results/boxplot_psa.png",
       plot = boxplot_psa,
       width = 10,
       height = 7)
