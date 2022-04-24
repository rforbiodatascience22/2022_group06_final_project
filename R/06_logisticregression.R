library("broom")
#Load data
data <- read_csv("data/03_dat_aug.csv")

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
data_nested_new <- data_nested %>% 
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

#See boxplot for mtDNA stratisfied on age to see if age affect Dfi
ggplot(data = data, map = aes(y = Age,
                              color = Dfi_class)) +
  geom_boxplot()


