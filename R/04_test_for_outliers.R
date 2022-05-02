library(tidyverse)
library(patchwork)
library(fs)


# Load augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")


# Create boxplot for each numeric variable to check for outliers ----------
age_plot <- data %>% 
  ggplot(aes(y = age,
             x = group_names,
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.15) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "Age",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


fat_intake_plot <- data %>% 
  ggplot(aes(y = dfi,
             x = group_names,
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.15) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "Daily dietary fat intake",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


bmi_plot <- data %>% 
  ggplot(aes(y = bmi,
             x = group_names,
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.15) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "BMI",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


psa_plot <- data %>% 
  ggplot(aes(y = psa,
             x = group_names,
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.15) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "Prostate specific antigen",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


mtdna_plot <- data %>% 
  ggplot(aes(y = mtdna,
             x = group_names,
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.15) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "mtDNA copy number",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


gleason_plot <- data %>% 
  ggplot(aes(y = gleason,
             x = "",
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.2,
              height = 0.2) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "Gleason score",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


ajcc_Plot <- data %>% 
  ggplot(aes(y = ajcc,
             x = "",
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.2,
              height = 0.2) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "AJCC Stage",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()  


tumor_Plot <- data %>% 
  ggplot(aes(y = tumor,
             x = "",
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.2,
              height = 0.2) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "Tumor severity",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


nodes_plot <- data %>% 
  ggplot(aes(y = lymph_nodes,
             x = "",
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.2,
              height = 0.2) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               show.legend = FALSE) + 
  labs(x = "Lymph nodes affected",
       y = "") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


metastasis_plot <- data %>% 
  ggplot(aes(y = metastasis,
             x = "",
             color = group_names)) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              width = 0.2,
              height = 0.2) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5) + 
  labs(x = "Metastasis",
       y = "",
       color="Group names") + 
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


boxplot_continuous <- age_plot + fat_intake_plot +
                      psa_plot + bmi_plot + mtdna_plot +
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the continuous attributes, to examine
                           distribution of outliers")

boxplot_discrete <- ajcc_plot + gleason_plot + tumor_plot +
                    nodes_plot + metastasis_plot + 
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the discrete attributes, to examine
                           distribution of outliers")
  
ggsave(filename = 'results/boxplot_continuous.png',
       plot = boxplot_continuous,
       width = 10,
       height = 6.5)

ggsave(filename = 'results/boxplot_discrete.png',
       plot = boxplot_discrete,
       width = 10,
       height = 6.5)