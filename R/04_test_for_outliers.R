# Load augmented data -----------------------------------------------------
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)

# Generate boxplots of all continous variables
age_plot <- data %>% 
  test_for_outliers(group_names, age, "Age", "")

fat_intake_plot <- data %>% 
  test_for_outliers(group_names, dfi, "Daily dietary fat intake", "")

bmi_plot <- data %>% 
  test_for_outliers(group_names, bmi, "BMI", "")

psa_plot <- data %>% 
  test_for_outliers(group_names, psa, "PSA", "")

mtdna_plot <- data %>% 
  test_for_outliers(group_names, mtdna, "mtDNA copy number", "")

# Generate plots of all discrete variables
gleason_plot <- data %>% 
  test_for_outliers(group_names, gleason, "Gleason score", "")

ajcc_plot <- data %>% 
  test_for_outliers(group_names, ajcc, "AJCC stage", "")

tumor_plot <- data %>% 
  test_for_outliers(group_names, tumor, "Tumor severity", "")

nodes_plot <- data %>% 
  test_for_outliers(group_names, lymph_nodes, "Lymph nodes affected", "")

metastasis_plot <- data %>% 
  test_for_outliers(group_names, metastasis, "Metastasis", "")


boxplot_continuous <- age_plot +
  fat_intake_plot +
  psa_plot +
  bmi_plot + 
  mtdna_plot +
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the continuous attributes, to examine distribution of outliers")

boxplot_discrete <- ajcc_plot +
  gleason_plot +
  tumor_plot +
  nodes_plot + 
  metastasis_plot + 
  plot_layout(ncol = 5) + 
  plot_annotation(title = "A plot of the discrete attributes, to examine distribution of outliers")
  
#Save plots to results
ggsave(filename = "results/boxplot_continuous.png",
       plot = boxplot_continuous,
       width = 10,
       height = 6.5)

ggsave(filename = "results/boxplot_discrete.png",
       plot = boxplot_discrete,
       width = 10,
       height = 6.5)