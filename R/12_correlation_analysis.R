source("R/99_func_file.R")

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

plt1 <- data %>% 
  correlation_analysis(age, mtdna, control = FALSE)

plt2 <- data %>% 
  correlation_analysis(age, mtdna, control = TRUE)

ggsave(filename = 'results/correlation_age_mtdna.png',
       plot = plt1,
       width = 10,
       height = 6.5)


