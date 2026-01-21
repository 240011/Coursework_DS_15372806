# =============================================================================
# EDA - HOUSE PRICE VISUALISATIONS
# ST5014CEM - Data Science for Developers
# Counties: Cheshire and Cumberland
# =============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)

# Set working directory path
base_path <- "."

# Load cleaned data
house_price_data <- read_csv(file.path(base_path, "cleaned_data/house_price_cleaned.csv"),
                             show_col_types = FALSE)
house_price_agg <- read_csv(file.path(base_path, "cleaned_data/house_price_aggregated.csv"),
                            show_col_types = FALSE)

# Create Charts directory if not exists
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# =============================================================================
# 1a) BOXPLOT: Average House Price (2023) - Cheshire vs Cumberland
# =============================================================================

hp_2023 <- house_price_data %>%
  filter(year == 2023)

plot_1a <- ggplot(hp_2023, aes(x = county, y = price, fill = county)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1) +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 1000000)) +
  scale_fill_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "House Price Distribution (2023)",
    subtitle = "Comparison between Cheshire and Cumberland",
    x = "County",
    y = "House Price (£)",
    fill = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/house_price_boxplot_2023.png"),
  plot = plot_1a,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Saved: house_price_boxplot_2023.png\n")

# =============================================================================
# 1b) BAR CHART: Average House Price (2024) - Both Counties
# =============================================================================

hp_2024_avg <- house_price_data %>%
  filter(year == 2024) %>%
  group_by(county) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

plot_1b <- ggplot(hp_2024_avg, aes(x = county, y = avg_price, fill = county)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_text(aes(label = paste0("£", format(round(avg_price), big.mark = ","))),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = label_comma(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Average House Price (2022)",
    subtitle = "Cheshire vs Cumberland",
    x = "County",
    y = "Average House Price (£)",
    fill = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/house_price_bar_2022.png"),
  plot = plot_1b,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Saved: house_price_bar_2024.png\n")

# =============================================================================
# 1c) LINE GRAPH: Average House Price Trend (2022-2024)
# =============================================================================

hp_trend <- house_price_data %>%
  group_by(county, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

plot_1c <- ggplot(hp_trend, aes(x = year, y = avg_price, color = county, group = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Average House Price Trend (2022-2024)",
    subtitle = "Comparison between Cheshire and Cumberland",
    x = "Year",
    y = "Average House Price (£)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/house_price_trend_2022_2024.png"),
  plot = plot_1c,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: house_price_trend_2022_2024.png\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== House Price Summary Statistics ===\n")
cat("\nBy County and Year:\n")
print(hp_trend)

cat("\n2024 Average Prices:\n")
print(hp_2024_avg)

cat("\nEDA House Price visualisations complete!\n")
