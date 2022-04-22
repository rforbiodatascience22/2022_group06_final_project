#load clean data
data <- read_csv(file = "data/02_dat_clean.csv")

#augmenting data to fit plot
data <- data %>% 
  select(Group, mtDNA) %>% 
  mutate(Group = case_when(Group == 1 ~ "PCa cases",
                            Group == 0 ~ "control"))

#recreating plot from article
ggplot(data,
       mapping = aes(x = Group,
                     y = mtDNA,
                     stratification = Group)) + 
  geom_boxplot() 
  
  


