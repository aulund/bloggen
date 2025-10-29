# Create advanced ggplot visualizations using the actual NBIS tutorial data
# Based on https://nbisweden.github.io/raukr-2023/labs/ggplot/

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("/Users/aulund/Documents/GitHub/bloggen/posts/day5")

# 1. ECONOMIST CORRUPTION AND HUMAN DEVELOPMENT SCATTER PLOT
# ===========================================================

# Read the economist data
ec <- read.csv("data_economist.csv", header = TRUE)
print("Loaded economist data:")
print(head(ec))

# Refactor the Region column as specified in the tutorial
ec$Region <- factor(ec$Region,
                    levels = c("EU W. Europe", "Americas", "Asia Pacific",
                              "East EU Cemt Asia", "MENA", "SSA"),
                    labels = c("OECD", "Americas", "Asia &\nOceania",
                              "Central &\nEastern Europe",
                              "Middle East &\nNorth Africa",
                              "Sub-Saharan\nAfrica"))

# Labels for specific countries to highlight
labels <- c("Congo", "Afghanistan", "Sudan", "Myanmar", "Iraq", "Venezuela", "Russia", 
           "Argentina", "Brazil", "Italy", "South Africa", "Cape Verde", "Bhutan", 
           "Botswana", "Britian", "New Zealand", "Greece", "China", "India", "Rwanda", 
           "Spain", "France", "United States", "Japan", "Norway", "Singapore", 
           "Barbados", "Germany")

# Create the corruption plot following the tutorial exactly
corruption_plot <- ggplot(ec, aes(x = CPI, y = HDI, color = Region)) +
  geom_smooth(aes(fill = "red"), method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "red", linewidth = 0.6) +
  geom_point(shape = 21, size = 3, stroke = 0.8, fill = "white") +
  geom_text(data = subset(ec, Country %in% labels), aes(label = Country),
            color = "black", size = 2.5, nudge_y = 0.02) +
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0.2, 1)) +
  scale_color_manual(values = c("#23576E", "#099FDB", "#29B00E", "#208F84", "#F55840", "#924F3E")) +
  scale_fill_manual(name = "trend", values = "red", labels = expression(paste(R^2, "=52%"))) +
  labs(title = "Corruption and human development",
       caption = "Sources: Transparency International; UN Human Development Report") +
  guides(color = guide_legend(nrow = 1)) +
  theme_bw() +
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

# Save the corruption plot
ggsave("tutorial_corruption_plot.png", corruption_plot, width = 10, height = 8, dpi = 300)

# 2. WSJ MEASLES HEATMAP
# ======================

# Read the WSJ measles data (skip 2 comment lines)
me <- read.csv("data_wsj.csv", header = TRUE, stringsAsFactors = FALSE, skip = 2)
print("Loaded WSJ measles data:")
print(head(me[,1:6]))  # Show first few columns

# Data tidying function as per tutorial
fun1 <- function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))

# Process the data following the tutorial exactly
me3 <- me %>%
  gather(key = state, value = value, -YEAR, -WEEK) %>%
  mutate(value = str_replace(value, "^-$", NA_character_),
         value = as.numeric(value)) %>%
  group_by(YEAR, state) %>% 
  summarise(total = fun1(value), .groups = "drop") %>%
  mutate(state = str_replace_all(state, "[.]", " "),
         state = str_to_title(state))

colnames(me3) <- tolower(colnames(me3))

# Custom colors as specified in the tutorial
cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", 
          "#ffd73e", "#eec73a", "#e29421", "#f05336", "#ce472e")

# Create the measles heatmap following the tutorial exactly
measles_plot <- ggplot(me3, aes(x = year, y = reorder(state, desc(state)), fill = total)) +
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
  geom_segment(x = 1963, xend = 1963, y = 0, yend = 51.5, 
               linewidth = 0.6, alpha = 0.7) +
  annotate("text", label = "Vaccine introduced", x = 1963, y = 53, 
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

# Save the measles heatmap
ggsave("tutorial_measles_heatmap.png", measles_plot, width = 12, height = 8, dpi = 300)

print("Tutorial plots created successfully!")
print("Files saved:")
print("1. tutorial_corruption_plot.png")
print("2. tutorial_measles_heatmap.png")