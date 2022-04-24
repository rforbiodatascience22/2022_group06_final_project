library(tidyverse)
library(broom)

#Load data
data <- read_csv(file = "data/03_dat_aug.csv")

PCA_data <- data %>%
  select(Group, Age, PSA, Dfi, BMI, mtDNA)

pca_fit <- PCA_data %>% 
  select(Age, PSA, Dfi, BMI, mtDNA) %>%
  prcomp(scale = TRUE)

pc1_vs_pc2 <- pca_fit %>%
  augment(PCA_data) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = as.character(Group))) +
  geom_point(size = 1.5) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom",
        plot.background = element_rect(colour = "black", fill=NA, size=1)) +
  labs(color = "Group")

ggsave(filename = "results/pc1_vs_pc2.png", 
       plot = pc1_vs_pc2,
       width = 10,
       height = 7)

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
pc1_pc2_weights <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, size = 2.5,
    color = "#904C2F") +
  theme_minimal(base_size = 20) +
  theme(plot.background = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "results/pc1_pc2_weights.png", 
       plot = pc1_pc2_weights,
       width = 10,
       height = 5)

pca_var_explained <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  filter(percent > 0.025) %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))) +
  theme_minimal(base_size = 20) +
  theme(plot.background = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "results/pca_var_explained.png", 
       plot = pca_var_explained,
       width = 10,
       height = 7)
