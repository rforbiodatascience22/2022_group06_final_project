library(tidyverse)
library(patchwork)



# read augmented data -----------------------------------------------------
data <- read_csv("data/03_dat_aug.csv")



# Create pointplot for variables on mtDNA and PSA -------------------------
PSA_AJCC <- ggplot(data = data) +
  geom_point(mapping = aes(x = PSA, y = AJCC)) +
  geom_smooth(se = FALSE)

mtDNA_AJCC <- ggplot(data = data) +
  geom_point(mapping = aes(x = mtDNA, y = AJCC))

AJCC_plot <- PSA_AJCC + mtDNA_AJCC +
  plot_annotation(title = "A plot showing the correlation between AJCC classification and the biomarkers 'PSA count' and 'mtDNA number count'")

AJCC_plot



