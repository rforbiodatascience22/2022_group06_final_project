library(tidyverse)
library(patchwork)
library(fs)

data <- read_csv("data/03_dat_aug.csv")

AgePlot <- data %>% 
  ggplot(mapping = aes(y = Age, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Age",
       y = "") + 
  theme_classic()

FatIntakePlot <- data %>% 
  ggplot(mapping = aes(y = Dfi, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Daily dietary fat intake",
       y = "") + 
  theme_classic()

BMIPlot <- data %>% 
  ggplot(mapping = aes(y = BMI, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "BMI",
       y = "") + 
  theme_classic()
  
AJCCPlot <- data %>% 
  ggplot(mapping = aes(y = AJCC, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "AJJC stage",
       y = "") + 
  theme_classic()

PSAPlot <- data %>% 
  ggplot(mapping = aes(y = PSA, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Prostate specific antigen",
       y = "") + 
  theme_classic()

GleasonPlot <- data %>% 
  ggplot(mapping = aes(y = Gleason, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Gleason score",
       y = "") + 
  theme_classic()

mtDNAPlot <- data %>% 
  ggplot(mapping = aes(y = mtDNA, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "mtDNA copy number",
       y = "") + 
  theme_classic()

TumorPlot <- data %>% 
  ggplot(mapping = aes(y = Tumor, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Tumor severity",
       y = "") + 
  theme_classic()

NodesPlot <- data %>% 
  ggplot(mapping = aes(y = LymphNodes, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Lymph nodes affected",
       y = "") + 
  theme_classic()

MetastasisPlot <- data %>% 
  ggplot(mapping = aes(y = Metastasis, x = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Metastasis",
       y = "") + 
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
