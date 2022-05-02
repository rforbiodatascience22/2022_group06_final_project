library(tidyverse)
library(patchwork)
library(rpart)
library(corrr)

# selection necessary for extraction of p.value 
data <- read_csv("data/03_dat_aug.csv") %>% 
  drop_na() %>% 
  select(-c("Sample","BMI_class","Dfi_class","TNM","Group_names",
            "Group","PCRsuccess"))

#correlation of all 
#data_control %>% 
#asd <- cor(data[ c("Age","Dfi","Smoking","PCaHist","BMI",
#                   "AJCC","PSA","Gleason","mtDNA",
#                   "Tumor","LymphNodes","Metastasis")])

preds01 <- predict(rpart(Tumor~LymphNodes,
                       data = data, 
                       method = "anova"), 
                 type = "vector")
tumorPlot01 <- data %>% 
  ggplot(aes(x = Tumor,
         y = LymphNodes)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) +
  geom_line(show.legend = FALSE, 
            aes(x=preds01,
                color = "red"))


preds02 <- predict(rpart(Tumor~Metastasis,
                       data = data, 
                       method = "anova"), 
                 type = "vector")
tumorPlot02 <- data %>% 
  ggplot(aes(x = Tumor,
             y = Metastasis)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds02,
                color = "red"))


preds03 <- predict(rpart(Tumor~mtDNA,
                       data = data, 
                       method = "anova"), 
                 type = "vector")
tumorPlot03 <- data %>% 
  ggplot(aes(x = Tumor,
             y = mtDNA)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds03,
                color = "red"))


preds04 <- predict(rpart(Tumor~PSA,
                         data = data, 
                         method = "anova"), 
                   type = "vector")  
tumorPlot04 <- data %>% 
  ggplot(aes(x = Tumor,
             y = PSA)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds04,
                color = "red"))


preds05 <- predict(rpart(Tumor~Gleason,
                         data = data, 
                         method = "anova"), 
                   type = "vector")  
tumorPlot05 <- data %>% 
  ggplot(aes(x = Tumor,
             y = Gleason)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) + 
  geom_line(show.legend = FALSE, 
            aes(x=preds05,
                color = "red"))


preds06 <- predict(rpart(Tumor~Gleason,
                         data = data, 
                         method = "anova"), 
                   type = "vector")  
tumorPlot06 <- data %>% 
  ggplot(aes(x = Tumor,
             y = AJCC)) +
  geom_jitter(alpha = 0.5,
              width = 0.15,
              height = 0.15) +
  geom_line(show.legend = FALSE, 
            aes(x=preds06,
                color = "red"))

bigTumorPlot <- (tumorPlot01 + tumorPlot02 + tumorPlot03) / 
  (tumorPlot04 + tumorPlot05 + tumorPlot06)

calc_ttest_p_value <- function(vec_a, vec_b){
  1 - t.test(vec_a, vec_b)$p.value
}

corrr::colpair_map(data, calc_ttest_p_value)
