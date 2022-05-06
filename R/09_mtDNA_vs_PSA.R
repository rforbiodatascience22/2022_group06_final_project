# Call support function ---------------------------------------------------
source("R/99_func_file.R")


# read augmented data -----------------------------------------------------
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

# Stratify on mtDNA levels and Gleason score ------------------------------
# Find mtDNA level median based on control group.
mtdna_control_median <- data %>% 
  filter(group_names == "control") %>% 
  summarise(median = median(mtdna)) %>% 
  unlist()

# Find mtDNA level median based on patient group.
mtdna_patient_median <- data %>% 
  filter(group_names == "prostate cancer") %>% 
  summarise(median = median(mtdna)) %>% 
  unlist()

# Augment data for upcoming plots. Four variables; higher/lower than control and
# patient median. Gleason class and AJCC class.
data <- data %>% 
  mutate(mtdna_group_c = case_when(mtdna <= mtdna_control_median ~ "low",
                                   mtdna > mtdna_control_median ~ "high"),
         mtdna_group_c = factor(mtdna_group_c,
                                levels = c("low", "high")),
         
         mtdna_group_p = case_when(mtdna <= mtdna_patient_median ~ "low",
                                   mtdna > mtdna_patient_median ~ "high"),
         mtdna_group_p = factor(mtdna_group_p,
                                levels = c("low", "high")),
         
         gleason_group = case_when(gleason <= 6 ~ "low",
                                   gleason == 7 ~ "medium",
                                   gleason >= 8 ~ "high"),
         gleason_group = factor(gleason_group,
                                levels = c("low", "medium", "high")),
         
         ajcc_stage = case_when(ajcc <= 2 ~ "II",
                                ajcc == 3 ~ "III",
                                ajcc == 4 ~ "IV"),
         ajcc_stage = factor(ajcc_stage,
                             levels = c("II", "III", "IV")),
         
         psa_level = case_when(psa < 10 ~ "<10",
                               psa >= 10 & psa < 20 ~ "10~20",
                               psa >= 20 ~ ">=20"),
         psa_level = factor(psa_level,
                            levels = c("<10", "10~20", ">=20")))




# Gleason bar charts stratified on mtDNA levels for both definitions. ------

## Control Median (like in the article).
# Call support function and group data, then pipe to plot.
gleason_plot_c <- data %>% 
  generate_09_plot_data(group1 = mtdna_group_c,
                        group2 = gleason_group) %>% 
  
  ggplot(mapping = aes(x = gleason_group,
                       y = perc*100,
                       fill = mtdna_group_c)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(x = "Gleason score",
       y = "(%)",
       fill = "mtDNA levels",
       title = "C) mtDNA levels based on control median", 
       subtitle = "Stratified on mtDNA levels <span style = 'color: #d95f02;'>
                  above the median</span> <br>and <span style ='color: 
                  #1b9e77;'>below the median</span>")

## Patient median (which might make more sense).
# Call support function and group data, then pipe to plot.
gleason_plot_p <- data %>%  
  generate_09_plot_data(group1 = mtdna_group_p,
                        group2 = gleason_group) %>% 
  
  ggplot(mapping = aes(x = gleason_group,
                       y = perc*100,
                       fill = mtdna_group_p)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(x = "Gleason score",
       y = "",
       fill = "mtDNA levels",
       title = "D) mtDNA levels based on patient median", 
       subtitle = "Stratified on mtDNA levels <span style = 'color: #d95f02;'>
                  above the median</span> <br>and <span style ='color: 
                  #1b9e77;'>below the median</span>")


# ADJJ bar charts stratified on mtDNA levels for both definitions. -----

## Control median (like in the article).
# Call support function and group data, then pipe to plot.
ajcc_plot_c <- data %>% 
  generate_09_plot_data(group1 = mtdna_group_c,
                        group2 = ajcc_stage) %>%
  
  ggplot(mapping = aes(x = ajcc_stage,
                       y = perc*100,
                       fill = mtdna_group_c)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(x = "AJCC stage",
       y = "(%)",
       fill = "mtDNA levels",
       title = "A) mtDNA levels based on control median", 
       subtitle = "Stratified on mtDNA levels <span style = 'color: #d95f02;'>
                  above the median</span> <br>and <span style ='color: 
                  #1b9e77;'>below the median</span>")


## Patient median (which might make more sense).
# Call support function and group data, then pipe to plot.
ajcc_plot_p <- data %>%
  generate_09_plot_data(group1 = mtdna_group_p,
                        group2 = ajcc_stage) %>%
  
  ggplot(mapping = aes(x = ajcc_stage,
                       y = perc*100,
                       fill = mtdna_group_p)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(x = "AJCC stage",
       y = "",
       fill = "mtDNA levels",
       title = "B) mtDNA levels based on patient median", 
       subtitle = "Stratified on mtDNA levels <span style = 'color: #d95f02;'>
                  above the median</span> <br>and <span style ='color: 
                  #1b9e77;'>below the median</span>")


# Plot PSA on Gleason score using article groups --------------------------
# Call support function and pipe to plot.
gleason_psa_agroup_plot <- data %>%  
  generate_09_plot_data(group1 = psa_level,
                        group2 = gleason_group) %>%
  
  ggplot(mapping = aes(x = gleason_group,
                       y = perc*100,
                       fill = psa_level)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(x = "Gleason score",
       y = "",
       fill = "PSA levels (ng/ml)",
       title = "F) PSA levels from article, stratified on Glea. score", 
       subtitle = "Stratified on PSA levels <span style = 'color: #1b9e77;'>
                  < 10</span>, <span style ='color: 
                  #d95f02;'>10~20</span> and <span style ='color: #7570b3;'>
                  20 <</span>")


# Plot PSA on AJCC using article groups ---------------------------------
# Call support function and pipe to plot.
ajcc_psa_agroup_plot <- data %>%  
  generate_09_plot_data(group1 = psa_level,
                        group2 = ajcc_stage) %>%
  
  ggplot(mapping = aes(x = ajcc_stage,
                       y = perc*100,
                       fill = psa_level)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(limits = c("II", "III", "IV")) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(x = "AJCC stage",
       y = "",
       fill = "PSA levels (ng/ml)",
       title = "E) PSA levels from article, stratified on AJCC stage", 
       subtitle = "Stratified on PSA levels <span style = 'color: #1b9e77;'>
                  < 10</span>, <span style ='color: 
                  #d95f02;'>10~20</span> and <span style ='color: #7570b3;'>
                  20 <</span>")


# Paste plots together with patchwork -----------------------------------

mtdna_vs_psa <- ajcc_plot_c +
  ajcc_plot_p +
  ajcc_psa_agroup_plot +
  gleason_plot_c +
  gleason_plot_p +
  gleason_psa_agroup_plot +
  labs(caption = "A-D, colored by mtDNA number counts in PBL's grouped above and below a median derived from control group (as in article) and patient group respectively.
  E-F colored by PSA levels measured in ng/ml. Every color adds up to 100 %.
  Stratification on AJCC stage: ('II' refers to both II A and II B). Stratification on Gleason score: ('low' is less than 7, 'medium' is 7 and 'high' is 8 or above)")

# Save plot to results.
ggsave(filename = "results/mtdna_vs_psa.png", 
       plot = mtdna_vs_psa,
       width = 14,
       height = 9)
