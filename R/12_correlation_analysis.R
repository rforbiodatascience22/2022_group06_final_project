
# Load data ---------------------------------------------------------------
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)


# Re-create statistical analysis from article -----------------------------
# The following analysis is based on the statistical analysis presented in the article

# Split data frame
data_control <- data %>% 
  filter(group_names == "control")
data_pca <- data %>% 
  filter(group_names == "pca_case")

# mtDNA vs. Age for controls  
plt1 <- data_control %>% 
  ggplot(mapping = aes(x = age, 
                       y = mtdna)) + 
  geom_point() 

plt1 + ggpubr::stat_cor(method = "spearman")

# mtDNA vs. Age for PCa cases
plt2 <- data_pca %>% 
  ggplot(mapping = aes(x = age, 
                       y = mtdna)) + 
  geom_point()  

plt2 + ggpubr::stat_cor(method = "spearman")


# mtDNA vs. BMI for controls
plt3 <- data_control %>% 
  ggplot(mapping = aes(x = bmi, 
                       y = mtdna)) + 
  geom_point()  

plt3 + ggpubr::stat_cor(method = "spearman")


# mtDNA vs. BMI for PCa cases
plt4 <- data_pca %>% 
  ggplot(mapping = aes(x = bmi, 
                       y = mtdna)) + 
  geom_point()  

plt4 + ggpubr::stat_cor(method = "spearman")

# #mtDNA vs. gleason score
# plt5 <- data_pca %>% 
#   mutate(gleason = factor(gleason)) %>% 
#   drop_na() %>% 
#   ggplot(mapping = aes(x = gleason, 
#                        y = mtdna,
#                        color = gleason)) + 
#   geom_point()   
# 
# plt5 + ggpubr::stat_cor(method = "spearman")
# 
# #PSA vs. Gleason
# plt6 <- data_pca %>% 
#   ggplot(mapping = aes(x = gleason, 
#                        y = psa)) + 
#   geom_point()   
# plt6 + ggpubr::stat_cor(method = "spearman")
