library(tidyverse)
library(patchwork)
library(fs)


# Load augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")


# Create boxplot for each numeric variable to check for outliers ----------
AgePlot <- data %>% 
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


FatIntakePlot <- data %>% 
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


BMIPlot <- data %>% 
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


PSAPlot <- data %>% 
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


mtDNAPlot <- data %>% 
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


GleasonPlot <- data %>% 
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


AJCCPlot <- data %>% 
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


TumorPlot <- data %>% 
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


NodesPlot <- data %>% 
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


MetastasisPlot <- data %>% 
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


Plot1 <- AgePlot + FatIntakePlot + PSAPlot + BMIPlot + mtDNAPlot +
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the nonbinary attributes, to examine distribution of outliers")

Plot2 <- AJCCPlot + GleasonPlot + TumorPlot + NodesPlot + MetastasisPlot + 
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the nonbinary attributes, to examine distribution of outliers")
  
ggsave(filename = 'results/OutlierPlot1.png',
       plot = Plot1,
       width = 10,
       height = 6.5)

ggsave(filename = 'results/OutlierPlot2.png',
       plot = Plot2,
       width = 10,
       height = 6.5)