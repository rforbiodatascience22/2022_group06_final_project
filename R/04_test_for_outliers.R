library(tidyverse)
library(patchwork)
library(fs)


# Load augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")


# Create boxplot for each numeric variable to check for outliers ----------
Age_Plot <- data %>% 
  ggplot(aes(y = Age,
             x = Group_names,
             color = Group_names)) +
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


Fat_Intake_Plot <- data %>% 
  ggplot(aes(y = Dfi,
             x = Group_names,
             color = Group_names)) +
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


BMI_Plot <- data %>% 
  ggplot(aes(y = BMI,
             x = Group_names,
             color = Group_names)) +
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


PSA_Plot <- data %>% 
  ggplot(aes(y = PSA,
             x = Group_names,
             color = Group_names)) +
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


mtDNA_Plot <- data %>% 
  ggplot(aes(y = mtDNA,
             x = Group_names,
             color = Group_names)) +
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


Gleason_Plot <- data %>% 
  ggplot(aes(y = Gleason,
             x = "",
             color = Group_names)) +
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


AJCC_Plot <- data %>% 
  ggplot(aes(y = AJCC,
             x = "",
             color = Group_names)) +
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


Tumor_Plot <- data %>% 
  ggplot(aes(y = Tumor,
             x = "",
             color = Group_names)) +
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


Nodes_Plot <- data %>% 
  ggplot(aes(y = LymphNodes,
             x = "",
             color = Group_names)) +
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


Metastasis_Plot <- data %>% 
  ggplot(aes(y = Metastasis,
             x = "",
             color = Group_names)) +
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


boxplot_continuous <- Age_Plot + Fat_Intake_Plot +
                      PSA_Plot + BMI_Plot + mtDNA_Plot +
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the nonbinary attributes, to examine
                           distribution of outliers")

boxplot_discrete <- AJCC_Plot + Gleason_Plot + Tumor_Plot +
                    Nodes_Plot + MetastasisPlot + 
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the nonbinary attributes, to examine
                           distribution of outliers")
  
ggsave(filename = 'results/boxplot_continuous.png',
       plot = boxplot_continuous,
       width = 10,
       height = 6.5)

ggsave(filename = 'results/boxplot_discrete.png',
       plot = boxplot_discrete,
       width = 10,
       height = 6.5)