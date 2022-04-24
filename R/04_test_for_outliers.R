library(tidyverse)
library(patchwork)
library(fs)

data <- read_csv("data/03_dat_aug.csv")

AgePlot <- data %>% 
  ggplot(mapping = aes(x = Age, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Age",
       y = "") + 
  theme_classic()

FatIntakePlot <- data %>% 
  ggplot(mapping = aes(x = Dfi, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Daily dietary fat intake",
       y = "") + 
  theme_classic()

BMIPlot <- data %>% 
  ggplot(mapping = aes(x = BMI, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "BMI",
       y = "") + 
  theme_classic()
  
AJCCPlot <- data %>% 
  ggplot(mapping = aes(x = AJCC, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "AJJC stage",
       y = "") + 
  theme_classic()

PSAPlot <- data %>% 
  ggplot(mapping = aes(x = PSA, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Prostate specific antigen",
       y = "") + 
  theme_classic()

GleasonPlot <- data %>% 
  ggplot(mapping = aes(x = Gleason, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Gleason score",
       y = "") + 
  theme_classic()

mtDNAPlot <- data %>% 
  ggplot(mapping = aes(x = mtDNA, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "mtDNA copy number",
       y = "") + 
  theme_classic()

TumorPlot <- data %>% 
  ggplot(mapping = aes(x = Tumor, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Tumor severity",
       y = "") + 
  theme_classic()

NodesPlot <- data %>% 
  ggplot(mapping = aes(x = LymphNodes, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Lymph nodes affected",
       y = "") + 
  theme_classic()

MetastasisPlot <- data %>% 
  ggplot(mapping = aes(x = Metastasis, y = "")) +
  geom_boxplot() + 
  geom_point(position = position_jitter(w = 0.1, h = 0.2),
             alpha = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  labs(x = "Metastasis",
       y = "") + 
  theme_classic()

BigPlot <- (AgePlot + FatIntakePlot + PSAPlot) / 
  (BMIPlot + AJCCPlot) /
  (GleasonPlot + mtDNAPlot) /
  (TumorPlot + NodesPlot + MetastasisPlot) +
  plot_annotation(title = "A plot of the nonbinary attributes, to examine distribution of outliers")

ggsave(filename = 'results/BigPlot.png',
       plot = BigPlot,
       width = 10,
       height = 6.5)
BigPlot
