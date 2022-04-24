library(tidyverse)
library(patchwork)
library(fs)

data <- read_csv("data/03_dat_aug.csv")

AgePlot <- data %>% 
  ggplot(mapping = aes(x = Age)) +
  geom_boxplot() + 
  labs(x = "Age",
       y = "")

FatIntakePlot <- data %>% 
  ggplot(mapping = aes(x = Dfi)) +
  geom_boxplot() + 
  labs(x = "Daily dietary fat intake",
       y = "")

BMIPlot <- data %>% 
  ggplot(mapping = aes(x = BMI)) +
  geom_boxplot() + 
  labs(x = "BMI",
       y = "")

AJCCPlot <- data %>% 
  ggplot(mapping = aes(x = AJCC)) +
  geom_boxplot() + 
  labs(x = "AJJC stage",
       y = "")

PSAPlot <- data %>% 
  ggplot(mapping = aes(x = PSA)) +
  geom_boxplot() + 
  labs(x = "Prostate specific antigen",
       y = "")

GleasonPlot <- data %>% 
  ggplot(mapping = aes(x = Gleason)) +
  geom_boxplot() + 
  labs(x = "Gleason score",
       y = "")

mtDNAPlot <- data %>% 
  ggplot(mapping = aes(x = mtDNA)) +
  geom_boxplot() + 
  labs(x = "mtDNA copy number",
       y = "")

TumorPlot <- data %>% 
  ggplot(mapping = aes(x = Tumor)) +
  geom_boxplot() + 
  labs(x = "Tumor severity",
       y = "")

NodesPlot <- data %>% 
  ggplot(mapping = aes(x = LymphNodes)) +
  geom_boxplot() + 
  labs(x = "Lymph nodes affected",
       y = "")

MetastasisPlot <- data %>% 
  ggplot(mapping = aes(x = Metastasis)) +
  geom_boxplot() + 
  labs(x = "Metastasis",
       y = "")

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

