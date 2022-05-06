# Re-creation of plot from article ----------------------------------------
# Load augmented data.
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

# Recreate article plot.
accurate_article_plot <- data %>% 
  ggplot(mapping = aes(x = group_names,
                       y = mtdna, 
                       fill  = group_names)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5) +  
  geom_boxplot(outlier.shape = 1,
               outlier.size = 5) +
  geom_signif(comparisons = list(c("control", "prostate cancer")), 
              map_signif_level = TRUE,
              size = 1) +
  theme_classic() + 
  theme(legend.position = "None",
        panel.background = element_rect(colour = "black",
                                        size=3)) + 
  labs(title = "Boxplot of relative mtDNA copy number of controls\nand cancer cases", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in 
Han chinese with prostate cancer and healthy controls.") +
  theme(plot.caption = element_text(size = 22,
                                    hjust = 0),
        text = element_text(size = 22)) +
  scale_y_continuous(limits = c(0, 2.5)) 


# Creating the plot for the presentation
article_plot <- data %>% 
  ggplot(mapping = aes(x = group_names,
                       y = mtdna, 
                       fill = group_names)) +
  geom_violin() +
  geom_boxplot(width = 0.05,
               color = "black",
               fill = "white",
               outlier.shape = NA) +
  geom_signif(comparisons = list(c("control", "prostate cancer")), 
              map_signif_level = TRUE,
              size = 1,
              color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() + 
  theme(legend.position = "None",
        panel.background = element_rect(colour = "black",
                                        size=3)) + 
  labs(title = "Boxplot of relative mtDNA copy number of controls\nand cancer cases", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in 
Han chinese with prostate cancer and healthy controls.") +
  theme(plot.caption = element_text(size = 22,
                                    hjust = 0),
        text = element_text(size = 22)) +
  scale_y_continuous(limits = c(0, 2.5)) +
  ylim(0, 2.8) 
  
# Add statistical test.
article_plot <- article_plot + 
  stat_compare_means(method = "t.test", 
                             size = 8,
                             hjust = -1.5,
                             vjust = 1)

# Save plot to results.
ggsave(filename = "results/article_visualization.png",
       plot = article_plot,
       width = 10,
       height = 10)
article_plot