# Function for logistic regression ----------------------------------------
logistic_regression <- function(include_psa = TRUE){
  
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
                               family = binomial(link = "logit"))))
    
  if (include_psa == FALSE) {
    data_nested <- data_nested %>% 
      mutate(mu_group = map(data,
                            ~glm(group ~ age + bmi + mtdna,
                                 data = .x,
                                 family = binomial(link = "logit"))))
  } 
  
    data_nested <- data_nested %>%
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

#Outlier detection function ----------------------------------------
test_for_outliers <- function(df, variable1, variable2, xlab, ylab){
  plt <- ggplot(df, mapping = aes(y = {{variable2}},
                                  x = {{variable1}},
                                  color = {{variable1}})) +
          geom_boxplot(outlier.shape = NA,
                       show.legend = FALSE) + 
          geom_jitter(show.legend = FALSE,
                      alpha = 0.5,
                      width = 0.15) + 
          stat_boxplot(geom = "errorbar",
                       width = 0.5,
                       show.legend = FALSE) + 
          labs(x = xlab,
               y = ylab) + 
          scale_color_brewer(palette = "Dark2") +
          theme_classic()
  return(plt)
}


#Function for correlation analysis ----------------------------------------
correlation_analysis <- function(df, variable1, variable2, control = TRUE){
  data_control <- df %>% 
    filter(group_names == "controls")
  data_pca <- df %>% 
    filter(group_names == "prostate cancer")
  
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



# Function for prepping data for plotting in 09 ---------------------------
# Filters data on patients, and groups on the compared/stratified variables.
# Output is a small summarized tibble with percentage-column.
generate_09_plot_data <- function(input_data, group1, group2){
  output <- input_data %>% 
    filter(group_names == "prostate cancer") %>%
    group_by({{group1}}, {{group2}}) %>% 
    summarize(count = n()) %>%
    drop_na() %>% 
    mutate(perc = count/sum(count))
  
  return(output)
}


