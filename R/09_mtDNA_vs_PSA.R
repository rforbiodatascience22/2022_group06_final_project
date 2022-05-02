library(tidyverse)
library("patchwork")
library(dplyr)



# read augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")



# Create pointplot for AJCC on mtDNA and PSA -------------------------
# PSA_AJCC <- data %>% 
#   ggplot(mapping = aes(x = PSA, y = AJCC)) +
#   geom_point() +
#   geom_smooth(se = FALSE)
# 
# mtDNA_AJCC <- data %>% 
#   ggplot(mapping = aes(x = mtDNA, y = AJCC)) +
#   geom_point() +
#   geom_smooth(se = FALSE)
# 
# AJCC_plot <- PSA_AJCC + mtDNA_AJCC +
#   plot_annotation(title = "A plot showing the correlation between AJCC classification and the biomarkers 'PSA count' and 'mtDNA number count'")
# 
# AJCC_plot
# 

# Create pointplot for Gleason on mtDNA and PSA ---------------------------
# PSA_Gleason <- data %>% 
#   ggplot(mapping = aes(x = PSA, y = Gleason)) +
#   geom_point() +
#   geom_smooth(se = FALSE)
# 
# mtDNA_Gleason <- data %>% 
#   ggplot(mapping = aes(x = mtDNA, y = Gleason)) +
#   geom_point() +
#   geom_smooth(se = FALSE)
# 
# Gleason_plot <- PSA_Gleason + mtDNA_Gleason +
#   plot_annotation(title = "A plot showing the correlation between Gleason classification and the biomarkers 'PSA count' and 'mtDNA number count'")
# 
# Gleason_plot



# Stratify on mtDNA levels and Gleason score -----------------------------------
mtDNA_controlmedian <- data %>% 
  filter(Group_names == "Controls") %>% 
  summarise(median = median(mtDNA)) %>% 
  print()

data <- data %>% 
  mutate(mtDNA_group_c = case_when(mtDNA > mtDNA_controlmedian[[1]] ~ "high",
                                 mtDNA <= mtDNA_controlmedian[[1]] ~ "low"))

mtDNA_patientmedian <- data %>% 
  filter(Group_names == "PCa cases") %>% 
  summarise(median = median(mtDNA)) %>% 
  print()

data <- data %>% 
  mutate(mtDNA_group_p = case_when(mtDNA > mtDNA_patientmedian[[1]] ~ "high",
                                   mtDNA <= mtDNA_patientmedian[[1]] ~ "low")) %>% 
  mutate(Gleason_group = case_when(Gleason <= 6 ~ "low",
                                   Gleason == 7 ~ "medium",
                                   Gleason >= 8 ~ "high"))


# Bar charts stratified on high and low mtDNA levels for both definitions. -----
# Group data for medians based on control group (like in the article)
plot_data <- data %>% 
  drop_na() %>%
  filter(Group_names == "PCa cases") %>%
  group_by(mtDNA_group_c, Gleason_group) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
Gleason_plot_c <- plot_data %>%  
  ggplot(mapping = aes(x = Gleason_group,
                       y = perc*100,
                       fill = mtDNA_group_c,
                       group = mtDNA_group_c)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  labs(x = "Gleason score",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on control median")

Gleason_plot_c 


# Group data based on the mtDNA median in patient group.
plot_data <- data %>% 
  drop_na() %>%
  filter(Group_names == "PCa cases") %>%
  group_by(mtDNA_group_p, Gleason_group) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
Gleason_plot_p <- plot_data %>%  
  ggplot(mapping = aes(x = Gleason_group,
                       y = perc*100,
                       fill = mtDNA_group_p,
                       group = mtDNA_group_p)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  labs(x = "Gleason score",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on patient median")

Gleason_plot_p


# Stratify on mtDNA levels and AJCC score -----------------------------------
data <- data %>% 
  mutate(AJCC_stage = case_when(AJCC <= 2 ~ "II",
                                AJCC == 3 ~ "III",
                                AJCC == 4 ~ "IV"))


# Bar charts stratified on high and low mtDNA levels for both definitions. -----

# Group data for medians besed on control group (like in the article)
plot_data <- data %>% 
  drop_na() %>%
  filter(Group_names == "PCa cases") %>%
  group_by(mtDNA_group_c, AJCC_stage) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
AJCC_plot_c <- plot_data %>%  
  ggplot(mapping = aes(x = AJCC_stage,
                       y = perc*100,
                       fill = mtDNA_group_c,
                       group = mtDNA_group_c)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("II", "III", "IV")) +
  labs(x = "AJCC stage",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on control median")

AJCC_plot_c 


# Group data based on the mtDNA median in patient group.
plot_data <- data %>% 
  drop_na() %>%
  filter(Group_names == "PCa cases") %>%
  group_by(mtDNA_group_p, AJCC_stage) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
AJCC_plot_p <- plot_data %>%  
  ggplot(mapping = aes(x = AJCC_stage,
                       y = perc*100,
                       fill = mtDNA_group_p,
                       group = mtDNA_group_p)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("II", "III", "IV")) +
  labs(x = "AJCC stage",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on patient median")

AJCC_plot_p



# Repeat, but for PSA levels ----------------------------------------------


# Introduce grouping of PSA levels ----------------------------------------
data <- data %>% 
  mutate(PSA_levels = case_when(PSA < 10 ~ "low",
                                PSA >= 10 & PSA < 20 ~ "medium",
                                PSA >= 20 ~ "high"))



# Plot PSA on Gleason score using article groups --------------------------








