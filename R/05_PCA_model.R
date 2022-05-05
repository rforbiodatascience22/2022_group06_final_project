# Load augmented data -----------------------------------------------------
data <- read_csv(file = "data/03_dat_aug.csv",
                 show_col_types = FALSE)


# Select data for PCA -----------------------------------------------------
pca_data <- data %>%
  select(group, age, psa, dfi, bmi, mtdna)



# Perform PCA -------------------------------------------------------------
pca_fit <- pca_data %>% 
  select(age, psa, dfi, bmi, mtdna) %>%
  prcomp(scale = TRUE)


# Plot PCA ----------------------------------------------------------------
pc1_vs_pc2 <- pca_fit %>%
  broom::augment(pca_data) %>% # add original dataset back in
  ggplot(mapping = aes(x = .fittedPC1, 
                       y = .fittedPC2, 
                       color = as.character(group))) +
  geom_point(size = 1.5) +
  theme_minimal(base_size = 19) +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black",
                                       fill = NA,
                                       size = 1),
        plot.title = ggtext::element_markdown()) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Group",
       x = "PC1",
       y = "PC2",
       title = "PC1 vs PC2 and split up for <span style = 'color:
                #d95f02;'>sick</span> and <span style =
                'color: #1b9e77;'>control</span>"
       )

# Save plot of PC1 vs PC2 in results.
ggsave(filename = "results/pc1_vs_pc2.png", 
       plot = pc1_vs_pc2,
       width = 10,
       height = 7)


# PCA weights -------------------------------------------------------------
# Define arrow style for plotting.
arrow_style <- arrow(angle = 20,
                     ends = "first",
                     type = "closed",
                     length = grid::unit(8, "pt"))

# Plot rotation matrix.
pc1_pc2_weights <- pca_fit %>%
  broom::tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value") %>%
  ggplot(mapping = aes(x = PC1,
                       y = PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(mapping = aes(label = column),
            hjust = 1, 
            nudge_x = -0.02, 
            size = 5.5,
            color = "#904C2F") +
  xlim(-0.8,0.5) +
  theme_minimal(base_size = 20) +
  theme(plot.background = element_rect(colour = "black", 
                                       fill=NA, size=1)) +
  labs(title = "PC1 and PC2 weights on indicator variables")

# Save plot to results.
ggsave(filename = "results/pc1_pc2_weights.png", 
       plot = pc1_pc2_weights,
       width = 10,
       height = 5)



# PC bar-plot -------------------------------------------------------------
pc_weights_bar <- pca_fit %>%
  broom::tidy(matrix = "rotation") %>%
  mutate(PC = as.character(PC)) %>%
  ggplot(mapping = aes(x = column, 
                       y = value, 
                       fill = PC)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Principal component weights on indicator variables",
       y = "Weight",
       x = "")

# Save plot to results.
ggsave(filename = "results/pc_weights_bar.png", 
       plot = pc_weights_bar,
       width = 8,
       height = 5)



# Plot of the explained variance ------------------------------------------
pca_var_explained <- pca_fit %>%
  broom::tidy(matrix = "eigenvalues") %>%
  filter(percent > 0.025) %>%
  ggplot(mapping = aes(x = PC,
                       y = percent)) +
  geom_col(fill = "#56B4E9", 
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_minimal(base_size = 19) +
  theme(plot.background = element_rect(colour = "black", 
                                       fill = NA,
                                       size = 1)) +
  labs(y = "Percent", 
       title = "Variance explained by each principle component")

# Save plot to results.
ggsave(filename = "results/pca_var_explained.png", 
       plot = pca_var_explained,
       width = 10,
       height = 7)
