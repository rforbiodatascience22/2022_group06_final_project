library(broom)


# Load augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")


# Data wrangling and regression -------------------------------------------
#Nest Group and all continuous variables
data_nested <- data %>%
  select(Group, Dfi_class, Age, BMI, PSA, mtDNA) %>% 
  group_by(Dfi_class) %>% 
  nest %>% 
  ungroup

#Perform logistic regression on Group vs all continuous variables
# stratisfied on Dfi_class
data_nested_PSA <- data_nested %>% 
  mutate(mu_group = map(data,
                        ~glm(Group ~ Age + BMI + PSA + mtDNA,
                             data = .x,
                             family = binomial(link = "logit"))))

#Extract estimates and p-values among others
data_nested_PSA <- data_nested_PSA %>% 
  # Make a new variable by extracting statistical values from model
  mutate(coef = map(mu_group,
                    ~tidy(.))) %>% 
  
  # Unnesting the coefficients
  unnest(coef) %>% 
  
  #Remove Intercept
  filter(term != "(Intercept)") %>% 
  
  #Remove columns not of interest
  select(-c(mu_group,
            std.error,
            statistic)) %>% 
  
  #Indicate whether significant or not
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   p.value >= 0.05 ~ "Not significant"))


# Save file
data_nested_PSA %>% 
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>% 
  write_rds(file = "results/Logistic_regression_PSA.rds")
#The above shows, that only PSA has a correlation with cancer.
# Except for people with Dfi_class = 1, then nothing was significant.
# More investigation into other factors,
# which could correlate with cancer in the below.


# Exclude PSA in regression -----------------------------------------------
#We see that PSA is a better predictor, but can mtDNA also contribute?
#Perform logistic regression on Group vs all continuous variables, except PSA
data_nested_mtDNA <- data_nested %>% 
  mutate(mu_group = map(data,
                        ~glm(Group ~ Age + BMI + mtDNA,
                             data = .x,
                             family = binomial(link = "logit"))))

#Extract estimates and p-values among others
data_nested_mtDNA <- data_nested_mtDNA %>% 
  # Make a new variable by extracting statistical values from model
  mutate(coef = map(mu_group,
                    ~tidy(.))) %>% 
  
  # Unnesting the coefficients
  unnest(coef) %>% 
  
  #Remove Intercept
  filter(term != "(Intercept)") %>% 
  
  #Remove columns not of interest
  select(-c(mu_group,
            std.error,
            statistic)) %>% 
  
  #Indicate whether significant or not
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   p.value >= 0.05 ~ "Not significant"))


# Save file
data_nested_mtDNA %>% 
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>% 
  write_rds(file = "results/Logistic_regression_mtDNA.rds")