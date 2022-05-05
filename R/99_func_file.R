# Function for logistic regression ----------------------------------------
logistic_regression <- function(psa = psa){
  
  data <- read_csv(file = "data/03_dat_aug.csv",
                   show_col_types = FALSE)
  
  data_nested <- data %>%
    select(group, dfi_class, age, bmi, psa, mtdna) %>% 
    group_by(dfi_class) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(mu_group = map(data,
                          ~glm(group ~ age + bmi + mtdna + psa,
                               data = .x,
                               family = binomial(link = "logit"),))) %>% 
    
    mutate(coef = map(mu_group,
                      ~broom::tidy(.))) %>% 
    unnest(coef) %>% 
    filter(term != "(Intercept)") %>% 
    select(-c(mu_group,
              std.error,
              statistic)) %>% 
    mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                     p.value >= 0.05 ~ "Not significant"))
  return(data_nested)
}
