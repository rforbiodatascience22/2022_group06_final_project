library(tidyverse)
library(ggpubr)
#load data
data <- read_csv(file = "data/02_dat_clean.csv")

#following analysis is based on the statistical analysis presented in the article

#split data frame
data_split <- data %>% 
  group_split(Group)

data_control <- as.data.frame(data_split[1])
data_PCa <- as.data.frame(data_split[2])

#mtDNA vs. Age for controls  
plt1 <- ggplot(data_control[[1]][[1]], mapping = aes(x = Age, 
                                                     y = mtDNA)) + 
  geom_point() 

plt1+ ggpubr::stat_cor(method = "spearman")

#mtDNA vs. Age for PCa cases
plt2 <- ggplot(data_PCa[[1]][[1]], mapping = aes(x = Age, 
                                                 y = mtDNA)) + 
  geom_point()  

plt2 + ggpubr::stat_cor(method = "spearman")


#mtDNA vs. BMI for controls
plt3 <- ggplot(data_control[[1]][[1]], mapping = aes(x = BMI, 
                                                     y = mtDNA)) + 
  geom_point()  

plt3 + ggpubr::stat_cor(method = "spearman")


#mtDNA vs. BMI for PCa cases
plt4 <- ggplot(data_PCa[[1]][[1]], mapping = aes(x = BMI, 
                                                 y = mtDNA)) + 
  geom_point()  

plt4 + ggpubr::stat_cor(method = "spearman")

#mtDNA vs. gleason score
plt5 <- ggplot(data_PCa[[1]][[1]], mapping = aes(x = Gleason, 
                                                 y = mtDNA,
                                                 color = Gleason)) + 
  geom_point()   

plt5 + ggpubr::stat_cor(method = "spearman")

#mtDNA vs. PSA
plt6 <- ggplot(data_PCa[[1]][[1]], mapping = aes(x = PSA, 
                                                 y = mtDNA)) + 
  geom_point()   
plt6 + ggpubr::stat_cor(method = "spearman")
