library(tidyverse)
library(broom)
library(ggtext)


# Load augmented data -----------------------------------------------------
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)


# Select data for PCA analysis --------------------------------------------
pca_data <- data %>%
  select(group, age, psa, dfi, bmi, mtdna)

pca_fit <- pca_data %>% 
  select(age, psa, dfi, bmi, mtdna) %>%
  prcomp(scale = TRUE)


# Perfoming PCA analysis --------------------------------------------------
pc1_vs_pc2 <- pca_fit %>%
  augment(pca_data) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, 
             .fittedPC2, 
             color = as.character(group))) +
  geom_point(size = 1.5) +
  theme_minimal(base_size = 19) +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black",
                                       fill = NA,
                                       size = 1),
        plot.title = element_markdown()) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Group",
       x = "PC1",
       y = "PC2",
       title = "PC1 vs PC2 and split up for <span style = 'color:
                #d95f02;'>sick</span> and <span style =
                'color: #1b9e77;'>control</span>"
       )

#Plot of PC1 vs PC2
ggsave(filename = "results/pc1_vs_pc2.png", 
       plot = pc1_vs_pc2,
       width = 10,
       height = 7)


# PCA weights -------------------------------------------------------------
# plot rotation matrix
# define arrow style for plotting
arrow_style <- arrow(angle = 20,
                     ends = "first",
                     type = "closed",
                     length = grid::unit(8, "pt")
)

pc1_pc2_weights <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, 
    nudge_x = -0.02, 
    size = 5.5,
    color = "#904C2F") +
  xlim(-0.8,0.5) +
  theme_minimal(base_size = 20) +
  theme(plot.background = element_rect(colour = "black", 
                                       fill=NA, size=1)) +
  labs(title = "PC1 and PC2 weights on indicator variables")

ggsave(filename = "results/pc1_pc2_weights.png", 
       plot = pc1_pc2_weights,
       width = 10,
       height = 5)

pc_weights_bar <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  mutate(across(PC, as.character)) %>%
  ggplot(mapping = aes(x = column, 
                       y = value, 
                       fill = PC)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(title = "Principal component weights on indicator variables",
       y = "Weight",
       x = ""
       )

ggsave(filename = "results/pc_weights_bar.png", 
       plot = pc_weights_bar,
       width = 8,
       height = 5)



# Plot of the explained variance ------------------------------------------
pca_var_explained <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  filter(percent > 0.025) %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", 
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))) +
  theme_minimal(base_size = 19) +
  theme(plot.background = element_rect(colour = "black", 
                                       fill=NA, size=1)) +
  labs(y = "Percent", 
       title = "Variance explained by each principle component"
       )

ggsave(filename = "results/pca_var_explained.png", 
       plot = pca_var_explained,
       width = 10,
       height = 7)
