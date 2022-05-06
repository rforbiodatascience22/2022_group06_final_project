# Correlation between fat intake and BMI ----------------------------------
# Load augmented data
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

# BMI stratified on 'dfi_class'.
boxplot_bmi_dfi <- data %>% 
  ggplot(mapping = aes(x = dfi_class,
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
                              "high fat")) +
  scale_fill_discrete(name = "DFI-class") +
  theme(text = element_text(size = 16)) +
  labs(x = "DFI-class",
       y = "BMI",
       title = "Boxplot of BMI stratified on DFI-class")

# Save plot to results.
ggsave(filename = "results/boxplot_bmi_dfi.png",
       plot = boxplot_bmi_dfi,
       width = 10,
       height = 7)
