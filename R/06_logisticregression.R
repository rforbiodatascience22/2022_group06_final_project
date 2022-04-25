library(broom)


# Load augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")


# Data wrangling and regression -------------------------------------------
#Nest Group and mtDNA
data_nested <- data %>%
  select(Dfi_class, Group, mtDNA) %>% 
  group_by(Dfi_class) %>% 
  nest %>% 
  ungroup

#Perform logistic regression on Group vs mtDNA stratisfied on Dfi_class
data_nested <- data_nested %>% 
  mutate(mu_group = map(data,
                        ~glm(Group ~ mtDNA,
                             data = .x,
                             family = binomial(link = "logit"))))

#Extract estimates and p-values among others
data_nested <- data_nested %>% 
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
data_nested %>% 
  select(-data) %>%
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>% 
  write_rds(file = "results/Logistic_regression.rds")
#The above shows, that mtDNA has a correlation with cancer.
# Except for people with Dfi_class = 3. More investigation to other factors,
# which could correlate with cancer in the below


# Further analysis of correlation between cancer and variables ------------
#See boxplot for mtDNA stratisfied on age to see if age affect Dfi
ggplot(data = data, map = aes(y = Age,
                              color = Dfi_class)) +
  geom_boxplot()
