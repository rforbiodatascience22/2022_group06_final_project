# Boxplot for PSA-levels --------------------------------------------------
# Load augmented data.
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

# Boxplot
boxplot_psa <- data %>% 
  ggplot(mapping = aes(x = group_names,
                       y = psa, 
                       color  = group_names)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5) +  
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.5,
              width = 0.1,
              height = 0.2) +
  geom_signif(comparisons = list(c("control", "pca_case")), 
              map_signif_level = TRUE,
              size = 0.4,
              color = "black") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") + 
  labs(title = "Boxplot of PSA of controls and prostate cancer cases", 
       y = "PSA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number
in Han chinese with prostate cancer and healthy controls.") +
  theme(text = element_text(size = 16),
        plot.caption = element_text(hjust = 0,
                                    face = "italic"))

#Add statistical test
boxplot_psa <- boxplot_psa +
  ggpubr::stat_compare_means(method = "t.test",
                             size = 4,
                             hjust = -1.15,
                             vjust = 1)

#Save figure
ggsave(filename = "results/boxplot_psa.png",
       plot = boxplot_psa,
       width = 10,
       height = 7)
