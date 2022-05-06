source("R/99_func_file.R")


# Logistic regression without 'psa' ---------------------------------------
logistic_regression(include_psa = FALSE) %>%
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>%  
  write_csv(file = "results/logistic_regression_mtdna.csv")

# Logistic regression with 'psa' ------------------------------------------
logistic_regression(include_psa = TRUE) %>%
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>% 
  write_csv(file = "results/logistic_regression_psa.csv")
