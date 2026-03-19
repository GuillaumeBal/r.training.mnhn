library(ggplot2)

# Create plots
p1 <- 
  ggplot(mtcars, aes(mpg, wt)) +
  geom_point() + labs(title = "Plot 1")
p2 <- 
  ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() + labs(title = "Plot 2")
p3 <- 
  ggplot(mtcars, aes(x = mpg)) +
  geom_histogram() + labs(title = "Plot 3")

# patchwork library =====================

library(patchwork)

# Combine plots
p1 + p2  # Side by side
p1 / p2  # Stacked vertically
(p1) / (p2 + p3) # Grid layout


# other package ================================

require(ggpubr)

ggarrange(
  ggarrange(p1, p2, ncol = 2),  # Inner arrangement
  p3,                             # Outer plot
  nrow = 2,
  labels = c("A", "B", "C")
)


