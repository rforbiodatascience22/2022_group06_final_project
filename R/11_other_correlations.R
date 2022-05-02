library(tidyverse)
library(patchwork)
library(rpart)
library(corrr)

# selection necessary for extraction of p.value 
data <- read_csv("data/03_dat_aug.csv") %>% 
  drop_na() %>% 
  select(-c(sample,bmi_class,dfi_class,tnm,group_names,
            group,pcr_success))

#correlation of all 
#data_control %>% 
#asd <- cor(data[ c("Age","Dfi","Smoking","PCaHist","BMI",
#                   "AJCC","PSA","Gleason","mtDNA",
#                   "Tumor","LymphNodes","Metastasis")])

preds01 <- predict(rpart(tumor~lymph_nodes,
                       data = data, 
                       method = "anova"), 
                 type = "vector")
tumor_plot01 <- data %>% 
  ggplot(aes(x = tumor,
         y = lymph_nodes)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) +
  geom_line(show.legend = FALSE, 
            aes(x=preds01,
                color = "red"))


preds02 <- predict(rpart(tumor~metastasis,
                       data = data, 
                       method = "anova"), 
                 type = "vector")
tumor_plot02 <- data %>% 
  ggplot(aes(x = tumor,
             y = metastasis)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds02,
                color = "red"))


preds03 <- predict(rpart(tumor~mtdna,
                       data = data, 
                       method = "anova"), 
                 type = "vector")
tumor_plot03 <- data %>% 
  ggplot(aes(x = tumor,
             y = mtdna)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds03,
                color = "red"))


preds04 <- predict(rpart(tumor~psa,
                         data = data, 
                         method = "anova"), 
                   type = "vector")  
tumor_plot04 <- data %>% 
  ggplot(aes(x = tumor,
             y = psa)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds04,
                color = "red"))


preds05 <- predict(rpart(tumor~gleason,
                         data = data, 
                         method = "anova"), 
                   type = "vector")  
tumor_plot05 <- data %>% 
  ggplot(aes(x = tumor,
             y = gleason)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds05,
                color = "red"))


preds06 <- predict(rpart(tumor~gleason,
                         data = data, 
                         method = "anova"), 
                   type = "vector")  
tumor_plot06 <- data %>% 
  ggplot(aes(x = tumor,
             y = ajcc)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) +
  geom_line(show.legend = FALSE, 
            aes(x=preds06,
                color = "red"))

big_tumor_plot <- (tumor_plot01 + tumor_plot02 + tumor_plot03) / 
  (tumor_plot04 + tumor_plot05 + tumor_plot06)

calc_ttest_p_value <- function(vec_a, vec_b){
  1 - t.test(vec_a, vec_b)$p.value
}

corrr::colpair_map(data, calc_ttest_p_value)
