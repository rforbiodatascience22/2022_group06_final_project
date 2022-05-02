library(tidyverse)
library("patchwork")
library(dplyr)



# read augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")

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
       y = "(%)",
       fill = "mtDNA levels",
       title = "mtDNA levels based on control median",
       caption = "Frequency plot of patients stratified on Gleason score ('low' is less than 7, 'medium' is 7 and 'high' is 8 or above).
       Colored by mtDNA count number in PBL relative to median derived from control group.
       Every color adds up to 100 %.")

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
       y = "(%)",
       fill = "mtDNA levels",
       title = "mtDNA levels based on patient median",
       caption = "Frequency plot of patients stratified on Gleason score ('low' is less than 7, 'medium' is 7 and 'high' is 8 or above).
       Colored by mtDNA count number in PBL relative to median derived from patient group.
       Every color adds up to 100 %.")

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
       y = "(%)",
       fill = "mtDNA levels",
       title = "mtDNA levels based on control median",
       caption = "Frequency plot of patients stratified on AJCC stage ('II' is both II A and II B). 
       Colored by mtDNA count number in PBL relative to median derived from control group.
       Every color adds up to 100 %.")

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
       y = "(%)",
       fill = "mtDNA levels",
       title = "mtDNA levels based on patient median",
       caption = "Frequency plot of patients stratified on AJCC stage ('II' is both II A and II B). 
       Colored by mtDNA count number in PBL relative to median derived from patient group.
       Every color adds up to 100 %.")

ajcc_plot_p



# Repeat, but for PSA levels ----------------------------------------------


# Introduce grouping of PSA levels ----------------------------------------
data <- data %>% 
  mutate(psa_level = case_when(psa < 10 ~ "<10",
                                psa >= 10 & psa < 20 ~ "10~20",
                                psa >= 20 ~ "20<"))

# Plot PSA on Gleason score using article groups --------------------------
plot_data <- data %>% 
  drop_na() %>%
  filter(group_names == "pca_cases") %>%
  group_by(psa_level, gleason_group) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# Plot

gleason_psa_agroup_plot <- plot_data %>%  
  ggplot(mapping = aes(x = gleason_group,
                       y = perc*100,
                       fill = psa_level,
                       group = psa_level)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  labs(x = "Gleason score",
       y = "(%)",
       fill = "PSA levels (ng/ml)",
       title = "PSA levels based on article groups, stratified on Gleason score",
       caption = "Frequency plot of patients stratified on Gleason score ('low' is less than 7, 'medium' is 7 and 'high' is 8 or above). 
       Colored by PSA levels measured in ng/ml in periferal blood. Every color adds up to 100 %.")

gleason_psa_agroup_plot



# Plot PSA on AJCC using article grouping ---------------------------------
plot_data <- data %>% 
  drop_na() %>%
  filter(group_names == "pca_cases") %>%
  group_by(psa_level, ajcc_stage) %>%
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

# Plot

ajcc_psa_agroup_plot <- plot_data %>%  
  ggplot(mapping = aes(x = ajcc_stage,
                       y = perc*100,
                       fill = psa_level,
                       group = psa_level)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("II", "III", "IV")) +
  labs(x = "AJCC stage",
       y = "(%)",
       fill = "PSA levels (ng/ml)",
       title = "PSA levels based on article groups, stratified on AJCC stage",
       caption = "Frequency plot of patients stratified on AJCC stage ('II' is stage II A and II B). 
       Colored by PSA levels measured in ng/ml in periferal blood. Every color adds up to 100 %.")

ajcc_psa_agroup_plot





