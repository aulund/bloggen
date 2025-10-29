# Create advanced ggplot visualizations based on NBIS tutorial
# for Day 5 blog post

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("/Users/aulund/Documents/GitHub/bloggen/posts/day5")

# 1. CORRUPTION AND HUMAN DEVELOPMENT SCATTER PLOT
# ================================================
# Create sample data similar to the Economist plot
set.seed(123)

# Create simpler sample data
n_countries <- 50
corruption_data <- data.frame(
  Country = paste("Country", 1:n_countries),
  CPI = runif(n_countries, 1, 10),
  HDI = runif(n_countries, 0.3, 1.0),
  Region = sample(c("OECD", "Americas", "Asia & Oceania", "Central & Eastern Europe",
                   "Middle East & North Africa", "Sub-Saharan Africa"), n_countries, replace = TRUE)
)

# Add correlation between CPI and HDI for realism
corruption_data$HDI <- 0.4 + 0.05 * corruption_data$CPI + rnorm(n_countries, 0, 0.1)
corruption_data$HDI <- pmax(0.3, pmin(1.0, corruption_data$HDI))

# Create the scatter plot
corruption_plot <- ggplot(corruption_data, aes(x = CPI, y = HDI, color = Region)) +
  geom_smooth(aes(fill = "red"), method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "red", linewidth = 0.6) +
  geom_point(shape = 21, size = 3, stroke = 0.8, fill = "white") +
  scale_x_continuous(name = "Corruption Perceptions Index, 2023 (10=least corrupt)",
                     breaks = 1:10, limits = c(0.5, 10)) +
  scale_y_continuous(name = "Human Development Index, 2023 (1=best)",
                     breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0.3, 1)) +
  scale_color_manual(values = c("#23576E", "#099FDB", "#29B00E", "#208F84", "#F55840", "#924F3E")) +
  scale_fill_manual(name = "trend", values = "red", labels = expression(paste(R^2, "=65%"))) +
  labs(title = "Corruption and Human Development",
       caption = "Sources: Transparency International; UN Human Development Report") +
  guides(color = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(face = "italic"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "grey60"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8))

ggsave("corruption_development_plot.png", corruption_plot, width = 10, height = 8, dpi = 300)

# 2. MEASLES HEATMAP
# ==================
# Create sample measles data similar to the WSJ plot
set.seed(456)
states <- c("Alaska", "Ark.", "Calif.", "Conn.", "Del.", "Ga.", "Iowa", "Kan.", "La.",
           "Md.", "Mass.", "Mich.", "Mo.", "Mont.", "N.D.", "Neb.", "N.M.", "N.Y.",
           "Okla.", "Pa.", "S.C.", "Tenn.", "Utah", "Vt.", "Wis.", "Wyo.")

# Generate data for years 1930-2010
measles_data <- expand.grid(
  year = 1930:2010,
  state = states
) %>%
  mutate(
    # Create realistic measles case patterns
    # High cases before 1963 (vaccine introduction), low after
    base_cases = ifelse(year < 1963, 
                       exp(rnorm(n(), mean = 4, sd = 1.5)), 
                       exp(rnorm(n(), mean = 1, sd = 1))),
    # Add some random variation
    cases = pmax(0, base_cases + rnorm(n(), 0, base_cases * 0.3)),
    # Cap maximum values
    cases = pmin(cases, 4000)
  ) %>%
  select(year, state, cases)

# Create custom color palette
cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", 
          "#ffd73e", "#eec73a", "#e29421", "#f05336", "#ce472e")

# Create the heatmap
measles_plot <- ggplot(measles_data, aes(x = year, y = reorder(state, desc(state)), fill = cases)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1930, 2010, by = 10)) +
  scale_fill_gradientn(colors = cols, 
                       na.value = "grey95",
                       limits = c(0, 4000),
                       values = c(0, 0.01, 0.02, 0.03, 0.09, 0.1, 0.15, 0.25, 0.4, 0.5, 1),
                       labels = c("0k", "1k", "2k", "3k", "4k"),
                       guide = guide_colourbar(ticks = TRUE, nbin = 50,
                                             barheight = 0.5, label = TRUE, 
                                             barwidth = 10)) +
  labs(x = "", y = "", fill = "", title = "Measles") +
  coord_fixed() +
  geom_segment(x = 1963, xend = 1963, y = 0, yend = 26.5, 
               linewidth = 0.6, alpha = 0.7) +
  annotate("text", label = "Vaccine introduced", x = 1963, y = 28, 
           vjust = 1, hjust = 0, size = 3) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.text = element_text(color = "grey20"),
        axis.text.y = element_text(size = 6, hjust = 1, vjust = 0.5),
        axis.text.x = element_text(size = 8),
        axis.ticks.y = element_blank(),
        title = element_text(hjust = -0.07, vjust = 1),
        panel.grid = element_blank())

ggsave("measles_heatmap.png", measles_plot, width = 12, height = 8, dpi = 300)

print("Advanced ggplot visualizations created:")
print("1. corruption_development_plot.png")
print("2. measles_heatmap.png")