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

#Function for correlation analysis ----------------------------------------
correlation_analysis <- function(df, variable1, variable2){
  
  plt <- ggplot(df, mapping = aes(x = {{variable1}}, 
                                  y = {{variable2}})) +
    
    geom_point(size = 3) 
  
  plt <- plt + ggpubr::stat_cor(method = "spearman")
    
  return(plt)
}


correlation_analysis1 <- function(df, variable1, variable2, control = TRUE){
  data_control <- df %>% 
    filter(group_names == "controls")
  data_pca <- df %>% 
    filter(group_names == "pca_cases")
  
  if (control == TRUE) {
    plt <- ggplot(data_control, mapping = aes(x = {{variable1}}, 
                                              y = {{variable2}})) +
      
      geom_point(size = 3) 
    
    plt <- plt + ggpubr::stat_cor(method = "spearman")
  }
  
  if (control == FALSE) {
    plt <- ggplot(data_pca, mapping = aes(x = {{variable1}}, 
                                          y = {{variable2}})) +
          
      geom_point(size = 3) 
    
    plt <- plt + ggpubr::stat_cor(method = "spearman")
  }
  
  return(plt)
}



