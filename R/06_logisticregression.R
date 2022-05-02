library(broom)


# Load augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")


# Data wrangling and regression -------------------------------------------
# Nest Group and all continuous variables
data_nested <- data %>%
  select(group, dfi_class, age, bmi, psa, mtdna) %>% 
  group_by(dfi_class) %>% 
  nest %>% 
  ungroup

# Perform logistic regression on Group vs all continuous variables
# stratified on Dfi_class
data_nested_psa <- data_nested %>% 
  mutate(mu_group = map(data,
                        ~glm(group ~ age + bmi + psa + mtdna,
                             data = .x,
                             family = binomial(link = "logit"))))

# Extract estimates and p-values among others
data_nested_psa <- data_nested_psa %>% 
  # Make a new variable by extracting statistical values from model
  mutate(coef = map(mu_group,
                    ~tidy(.))) %>% 
  
  # Unnesting the coefficients
  unnest(coef) %>% 
  
  # Remove Intercept
  filter(term != "(Intercept)") %>% 
  
  # Remove columns not of interest
  select(-c(mu_group,
            std.error,
            statistic)) %>% 
  
  # Indicate whether significant or not
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   p.value >= 0.05 ~ "Not significant"))


# Save file
data_nested_psa %>% 
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>% 
  write_csv(file = "results/Logistic_regression_psa.csv")
# The above shows, that only PSA has a correlation with cancer.
# Except for people with dfi_class = 1, then nothing was significant.
# More investigation into other factors, which could correlate with
# cancer are done below.


# Exclude PSA in regression -----------------------------------------------
# We see that PSA is a better predictor, but can mtDNA also contribute?
# Perform logistic regression on Group vs all continuous variables, except PSA
data_nested_mtdna <- data_nested %>% 
  mutate(mu_group = map(data,
                        ~glm(group ~ age + bmi + mtdna,
                             data = .x,
                             family = binomial(link = "logit"))))

# Extract estimates and p-values among others
data_nested_mtdna <- data_nested_mtdna %>% 
  # Make a new variable by extracting statistical values from model
  mutate(coef = map(mu_group,
                    ~tidy(.))) %>% 
  
  # Unnesting the coefficients
  unnest(coef) %>% 
  
  # Remove Intercept
  filter(term != "(Intercept)") %>% 
  
  # Remove columns not of interest
  select(-c(mu_group,
            std.error,
            statistic)) %>% 
  
  # Indicate whether significant or not
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   p.value >= 0.05 ~ "Not significant"))


# Save file
data_nested_mtdna %>% 
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>% 
  write_csv(file = "results/logistic_regression_mtdna.csv")