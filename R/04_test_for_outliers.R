library(tidyverse)
library(patchwork)
library(fs)

data = read_csv("data/03_dat_aug.csv")

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
  labs(x = "AJJC",
       y = "")

PSAPlot <- data %>% 
  ggplot(mapping = aes(x = PSA)) +
  geom_boxplot() + 
  labs(x = "PSA",
       y = "")

GleasonPlot <- data %>% 
  ggplot(mapping = aes(x = Gleason)) +
  geom_boxplot() + 
  labs(x = "Gleason",
       y = "")

mtDNAPlot <- data %>% 
  ggplot(mapping = aes(x = mtDNA)) +
  geom_boxplot() + 
  labs(x = "mtDNA",
       y = "")

BigPlot <- (AgePlot + FatIntakePlot) / 
  (BMIPlot + AJCCPlot) /
  (GleasonPlot + mtDNAPlot) + 
  plot_annotation(title = "A plot of the nonbinary attributes, to examine distribution of outliers")

BigPlot
