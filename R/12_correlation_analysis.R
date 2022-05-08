# Load data ---------------------------------------------------------------
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

#test difference in correlation between age and 
#mtDNA in controls and prostate cancer cases

plt1 <- data %>% 
  correlation_analysis(age, mtdna, control = FALSE)

plt2 <- data %>% 
  correlation_analysis(age, mtdna, control = TRUE)

ggsave(filename = 'results/correlation_age_mtdna.png',
       plot = plt1 + plt2,
       width = 10,
       height = 6.5)


