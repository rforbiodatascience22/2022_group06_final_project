library(tidyverse)
library(ggpubr)
library(ggsignif)
# Re-creation of plot from article ----------------------------------------
# Load augmented data
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

#Article plot
article_plot <- ggplot(data, mapping = aes(x = group_names,
                           y = mtdna, 
                           fill  = group_names)) + 
  
  stat_boxplot(geom = "errorbar",
               width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  geom_signif(comparisons = list(c("controls",
                                   "pca_cases")), 
              map_signif_level = TRUE,
              size = 1) +
  theme(legend.position = "None") + 
  labs(title = "Boxplot of relative mtDNA copy number
                of controls and PCa cases", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in 
Han chinese with prostate cancer and healthy controls.") +
  theme(plot.caption = element_text(size = 22,
                            hjust = 0)) + 
  ylim(0, 2.8)

#Add statistical test
article_plot <- article_plot + 
  ggpubr::stat_compare_means(method = "t.test", 
                             size = 8)
article_plot
#Save figure
ggsave(filename = "results/article_visualization.png",
       plot = article_plot,
       width = 10,
       height = 10)
