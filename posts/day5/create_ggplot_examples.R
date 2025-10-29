# Create example ggplot2 visualizations for Day 5 blog post
# Install required packages if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(ggplot2)
library(dplyr)

# Set working directory to save plots
setwd("/Users/aulund/Documents/GitHub/bloggen/posts/day5")

# Example 1: Gene Expression Scatter Plot
set.seed(123)
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:100),
  control = rnorm(100, mean = 5, sd = 2),
  treatment = rnorm(100, mean = 6, sd = 2.5),
  significance = sample(c("Significant", "Not Significant"), 100, replace = TRUE, prob = c(0.3, 0.7))
)

gene_data$log2_fold_change <- log2(gene_data$treatment / gene_data$control)
gene_data$avg_expression <- (gene_data$control + gene_data$treatment) / 2

# Create MA plot
p1 <- ggplot(gene_data, aes(x = avg_expression, y = log2_fold_change, color = significance)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Significant" = "#E31A1C", "Not Significant" = "#1F78B4")) +
  labs(title = "MA Plot: Gene Expression Analysis",
       subtitle = "Differential expression between control and treatment",
       x = "Average Expression (log2)",
       y = "Log2 Fold Change",
       color = "Statistical\nSignificance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom")

ggsave("gene_expression_ma_plot.png", p1, width = 8, height = 6, dpi = 300)

# Example 2: Quality Control Metrics
qc_data <- data.frame(
  sample_id = paste0("Sample_", 1:24),
  read_count = sample(20000000:80000000, 24),
  gc_content = rnorm(24, mean = 42, sd = 3),
  duplication_rate = runif(24, 5, 25),
  batch = rep(c("Batch_A", "Batch_B", "Batch_C"), each = 8)
)

# Create faceted quality control plot
p2 <- qc_data %>%
  ggplot(aes(x = gc_content, y = duplication_rate, color = batch, size = read_count/1000000)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~batch, ncol = 3) +
  scale_size_continuous(name = "Read Count\n(Millions)", range = c(2, 8)) +
  scale_color_viridis_d(name = "Batch") +
  labs(title = "Quality Control Metrics Across Batches",
       subtitle = "GC content vs duplication rate, sized by read count",
       x = "GC Content (%)",
       y = "Duplication Rate (%)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(face = "bold"))

ggsave("qc_metrics_faceted.png", p2, width = 10, height = 4, dpi = 300)

# Example 3: Time series plot (could represent sequencing runs over time)
time_data <- data.frame(
  date = seq(as.Date("2025-01-01"), as.Date("2025-10-29"), by = "week"),
  successful_runs = rpois(44, lambda = 8),
  failed_runs = rpois(44, lambda = 2)
) %>%
  mutate(total_runs = successful_runs + failed_runs,
         success_rate = successful_runs / total_runs * 100)

p3 <- ggplot(time_data, aes(x = date)) +
  geom_line(aes(y = successful_runs, color = "Successful"), size = 1.2) +
  geom_line(aes(y = failed_runs, color = "Failed"), size = 1.2) +
  geom_point(aes(y = successful_runs, color = "Successful"), size = 2) +
  geom_point(aes(y = failed_runs, color = "Failed"), size = 2) +
  scale_color_manual(values = c("Successful" = "#2E8B57", "Failed" = "#DC143C")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(title = "Sequencing Run Success Rate Over Time",
       subtitle = "Tracking laboratory performance throughout 2025",
       x = "Date",
       y = "Number of Runs",
       color = "Run Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("sequencing_timeline.png", p3, width = 10, height = 6, dpi = 300)

print("All plots have been created and saved!")
print("Files created:")
print("1. gene_expression_ma_plot.png")
print("2. qc_metrics_faceted.png") 
print("3. sequencing_timeline.png")