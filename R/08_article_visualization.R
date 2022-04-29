library(tidyverse)
library(ggpubr)
# Re-creation of plot from article ----------------------------------------
# Load augmented data
data <- read_csv(file = "data/03_dat_aug.csv")

#Recreating plot from article
plt1 <- ggplot(data, mapping = aes(x = Group_names,
                           y = mtDNA,
                           stratification = Group_names, 
                           fill  = Group_names)) + 
  
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  geom_signif(comparisons = list(c("Controls", "PCa cases")), 
              map_signif_level=TRUE, size = 0.4) +
  theme(legend.position = "right") + 
  labs(title = "Boxplot of relative mtDNA copy number of controls and PCa cases", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in Han chinese with prostate cancer and healthy controls.") +
  theme(plot.title = element_text(size = 10), 
        plot.caption = element_text(size = 6),
        plot.caption.position = "plot") + 
  ylim(0, 2.8)

plt1 + ggpubr::stat_compare_means(method = "t.test", size = 3)

ggsave(filename = "results/article_visualization.png",
       plot = plt1,
       width = 10,
       height = 7)

plt1 + ggpubr::stat_compare_means(method = "t.test", size = 3)


plt2 <- ggplot(data, mapping = aes(x = Group_names,
                                  y = PSA,
                                  stratification = Group_names, 
                                  fill  = Group_names)) + 
  
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  geom_signif(comparisons = list(c("Controls", "PCa cases")), 
              map_signif_level=TRUE, size = 0.4) +
  theme(legend.position = "right") + 
  labs(title = "Boxplot of PSA of controls and PCa cases", 
       y = "PSA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in Han chinese with prostate cancer and healthy controls.") +
  theme(plot.title = element_text(size = 10), 
        plot.caption = element_text(size = 6),
        plot.caption.position = "plot")  

ggsave(filename = "results/boxplot_PSA.png",
       plot = plt2,
       width = 10,
       height = 7)

plt2 + ggpubr::stat_compare_means(method = "t.test", size = 3)


