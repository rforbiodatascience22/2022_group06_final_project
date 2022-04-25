#load clean data
data <- read_csv(file = "data/02_dat_clean.csv")

#augmenting data to fit plot
data <- data %>% 
  select(Group, mtDNA) %>% 
  mutate(Group = case_when(Group == 1 ~ "PCa cases",
                            Group == 0 ~ "Controls"))

#recreating plot from article
ggplot(data,
       mapping = aes(x = Group,
                     y = mtDNA,
                     stratification = Group, 
                     fill  = Group)) + 
  #stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(outlier.shape = 1) +
  theme_classic() +
  theme(legend.position = "right") + 
  labs(title = "Boxplot of relative mtDNA copy number of controls and PCa cases", 
       y = "mtDNA", 
       x = "Group", 
       caption = "Figure: Distribution of pheripheral blood mtDNA copy number in Han chinese with prostate cancer and healthy controls.") +
  theme(plot.title = element_text(size = 10), plot.caption = element_text(size = 5))

#performing t.test of the groups
data %>% split(f = "Group")
    
  


