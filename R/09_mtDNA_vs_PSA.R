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
mtdna_control_median <- data %>% 
  filter(group_names == "controls") %>% 
  summarise(median = median(mtdna)) %>% 
  print()

data <- data %>% 
  mutate(mtdna_group_c = case_when(mtdna > mtdna_control_median[[1]] ~ "high",
                                   mtdna <= mtdna_control_median[[1]] ~ "low"))

mtdna_patient_median <- data %>% 
  filter(group_names == "pca_cases") %>% 
  summarise(median = median(mtdna)) %>% 
  print()

data <- data %>% 
  mutate(mtdna_group_p = case_when(mtdna > mtdna_patient_median[[1]] ~ "high",
                                   mtdna <= mtdna_patient_median[[1]] ~ "low")) %>% 
  mutate(gleason_group = case_when(gleason <= 6 ~ "low",
                                   gleason == 7 ~ "medium",
                                   gleason >= 8 ~ "high"))


# Bar charts stratified on high and low mtDNA levels for both definitions. -----
# Group data for medians based on control group (like in the article)
plot_data <- data %>% 
  drop_na() %>%
  filter(group_names == "pca_cases") %>%
  group_by(mtdna_group_c, gleason_group) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
gleason_plot_c <- plot_data %>%  
  ggplot(mapping = aes(x = gleason_group,
                       y = perc*100,
                       fill = mtdna_group_c,
                       group = mtdna_group_c)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  labs(x = "Gleason score",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on control median")

gleason_plot_c 


# Group data based on the mtDNA median in patient group.
plot_data <- data %>% 
  drop_na() %>%
  filter(group_names == "pca_cases") %>%
  group_by(mtdna_group_p, gleason_group) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
gleason_plot_p <- plot_data %>%  
  ggplot(mapping = aes(x = gleason_group,
                       y = perc*100,
                       fill = mtdna_group_p,
                       group = mtdna_group_p)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  labs(x = "Gleason score",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on patient median")

gleason_plot_p


# Stratify on mtDNA levels and AJCC score -----------------------------------
data <- data %>% 
  mutate(ajcc_stage = case_when(ajcc <= 2 ~ "II",
                                ajcc == 3 ~ "III",
                                ajcc == 4 ~ "IV"))


# Bar charts stratified on high and low mtDNA levels for both definitions. -----

# Group data for medians besed on control group (like in the article)
plot_data <- data %>% 
  drop_na() %>%
  filter(group_names == "pca_cases") %>%
  group_by(mtdna_group_c, ajcc_stage) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
ajcc_plot_c <- plot_data %>%  
  ggplot(mapping = aes(x = ajcc_stage,
                       y = perc*100,
                       fill = mtdna_group_c,
                       group = mtdna_group_c)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("II", "III", "IV")) +
  labs(x = "AJCC stage",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on control median")

ajcc_plot_c 


# Group data based on the mtDNA median in patient group.
plot_data <- data %>% 
  drop_na() %>%
  filter(group_names == "pca_cases") %>%
  group_by(mtdna_group_p, ajcc_stage) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot
ajcc_plot_p <- plot_data %>%  
  ggplot(mapping = aes(x = ajcc_stage,
                       y = perc*100,
                       fill = mtdna_group_p,
                       group = mtdna_group_p)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("II", "III", "IV")) +
  labs(x = "AJCC stage",
       y = "percentage",
       fill = "mtDNA levels",
       title = "mtDNA levels based on patient median")

ajcc_plot_p



# Repeat, but for PSA levels ----------------------------------------------


# Introduce grouping of PSA levels ----------------------------------------
data <- data %>% 
<<<<<<< HEAD
  mutate(PSA_level = case_when(PSA < 10 ~ "<10",
                                PSA >= 10 & PSA < 20 ~ "10~20",
                                PSA >= 20 ~ "20<"))
=======
  mutate(psa_levels = case_when(psa < 10 ~ "low",
                                psa >= 10 & psa < 20 ~ "medium",
                                psa >= 20 ~ "high"))

>>>>>>> 2d290be77693b2dbcfbd4245d418331b1caf75e9


# Plot PSA on Gleason score using article groups --------------------------
plot_data <- data %>% 
  drop_na() %>%
  filter(Group_names == "PCa cases") %>%
  group_by(PSA_level, Gleason_group) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot

gleason_psa_agroup_plot <- plot_data %>%  
  ggplot(mapping = aes(x = Gleason_group,
                       y = perc*100,
                       fill = PSA_level,
                       group = PSA_level)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  labs(x = "Gleason score",
       y = "(%)",
       fill = "PSA levels (ng/ml)",
       title = "PSA levels based on article groups",
       caption = "Frequency plot of patients stratified on Gleason score ('low' is less than 7, 'medium' is 7 and 'high' is 8 or above). 
       Colored by PSA levels measured in ng/ml in periferal blood. Every color adds up to 100 %.")

gleason_psa_agroup_plot



# TEST ZONE ---------------------------------------------------------------

data %>% 
  drop_na() %>%
  filter(Group_names == "PCa cases") %>%
  group_by(PSA_level, Gleason_group) %>%
  count()




