# Create visualizations from the original ggplot examples in the blog post
# Using the iris dataset as in the NBIS tutorial
library(ggplot2)

# Set working directory to save plots
setwd("/Users/aulund/Documents/GitHub/bloggen/posts/day5")

# Use the built-in iris dataset
data("iris")

# Example 1: Basic scatter plot from the original code using iris
p1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_minimal() +
  labs(title = "My Analysis Results",
       x = "Petal Length", 
       y = "Petal Width")

ggsave("basic_scatter_plot.png", p1, width = 8, height = 6, dpi = 300)

# Example 2: Advanced plotting with facets from the original code using iris
p2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_bw()

ggsave("faceted_scatter_plot.png", p2, width = 10, height = 6, dpi = 300)

print("Original example plots created using iris dataset:")
print("1. basic_scatter_plot.png")
print("2. faceted_scatter_plot.png")