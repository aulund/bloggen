# Create visualizations from the original ggplot examples in the blog post
library(ggplot2)

# Set working directory to save plots
setwd("/Users/aulund/Documents/GitHub/bloggen/posts/day5")

# Create sample dataset to match the original examples
set.seed(123)
dataset <- data.frame(
  variable1 = rnorm(50, mean = 10, sd = 3),
  variable2 = rnorm(50, mean = 15, sd = 4) + rnorm(50, mean = 0, sd = 1),
  group = sample(c("Group A", "Group B", "Group C"), 50, replace = TRUE),
  category = sample(c("Category 1", "Category 2"), 50, replace = TRUE)
)

# Example 1: Basic scatter plot from the original code
p1 <- ggplot(data = dataset, aes(x = variable1, y = variable2)) +
  geom_point() +
  theme_minimal() +
  labs(title = "My Analysis Results",
       x = "X-axis Label",
       y = "Y-axis Label")

ggsave("basic_scatter_plot.png", p1, width = 8, height = 6, dpi = 300)

# Example 2: Advanced plotting with facets from the original code
p2 <- ggplot(data = dataset, aes(x = variable1, y = variable2, color = group)) +
  geom_point() +
  facet_wrap(~category) +
  theme_bw()

ggsave("faceted_scatter_plot.png", p2, width = 10, height = 6, dpi = 300)

print("Original example plots created:")
print("1. basic_scatter_plot.png")
print("2. faceted_scatter_plot.png")