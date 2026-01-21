library(tidyverse)
library(ggplot2)
library(scales)

# Set working directory path
base_path <- "."

# Load cleaned data
broadband_data <- read_csv(file.path(base_path, "cleaned_data/broadband_cleaned.csv"),
                           show_col_types = FALSE)
broadband_agg <- read_csv(file.path(base_path, "cleaned_data/broadband_aggregated.csv"),
                          show_col_types = FALSE)

# Create Charts directory if not exists
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# =============================================================================
# BOXPLOT: Average Download Speed - Cheshire
# =============================================================================

broadband_cheshire <- broadband_data %>%
  filter(county == "Cheshire")

# Create custom color palette for Cheshire towns
n_towns_cheshire <- length(unique(broadband_cheshire$town))
cheshire_colors <- colorRampPalette(c("#FF6B6B", "#FFA07A", "#FFB6C1"))(n_towns_cheshire)

plot_2a_cheshire <- ggplot(broadband_cheshire, aes(x = town, y = avg_download_speed, fill = town)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_fill_manual(values = cheshire_colors) +
  labs(
    title = "Average Download Speed Distribution - Cheshire",
    subtitle = "By Local Authority Area",
    x = "Town/Local Authority",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/broadband_boxplot_cheshire.png"),
  plot = plot_2a_cheshire,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: broadband_boxplot_cheshire.png\n")

# =============================================================================
# BOXPLOT: Average Download Speed - Cumberland
# =============================================================================

broadband_cumberland <- broadband_data %>%
  filter(county == "Cumberland")

# Create custom color palette for Cumberland towns
n_towns_cumberland <- length(unique(broadband_cumberland$town))
cumberland_colors <- colorRampPalette(c("#4ECDC4", "#45B7D1", "#96CEB4"))(n_towns_cumberland)

plot_2a_cumberland <- ggplot(broadband_cumberland, aes(x = town, y = avg_download_speed, fill = town)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_fill_manual(values = cumberland_colors) +
  labs(
    title = "Average Download Speed Distribution - Cumberland",
    subtitle = "By Local Authority Area",
    x = "Town/Local Authority",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/broadband_boxplot_cumberland.png"),
  plot = plot_2a_cumberland,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: broadband_boxplot_cumberland.png\n")

# =============================================================================
# STACKED BAR CHART: Average vs Maximum Speed - Cheshire
# =============================================================================

broadband_cheshire_agg <- broadband_agg %>%
  filter(county == "Cheshire") %>%
  pivot_longer(
    cols = c(avg_download_speed, max_download_speed),
    names_to = "speed_type",
    values_to = "speed"
  ) %>%
  mutate(
    speed_type = case_when(
      speed_type == "avg_download_speed" ~ "Average Speed",
      speed_type == "max_download_speed" ~ "Maximum Speed"
    )
  )

plot_2b_cheshire <- ggplot(broadband_cheshire_agg, aes(x = town, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_y_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_fill_manual(values = c("Average Speed" = "#FF6B6B", "Maximum Speed" = "#FFD93D")) +
  labs(
    title = "Broadband Speed by Town - Cheshire",
    subtitle = "Average vs Maximum Download Speed",
    x = "Town/Local Authority",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/broadband_stacked_bar_cheshire.png"),
  plot = plot_2b_cheshire,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: broadband_stacked_bar_cheshire.png\n")

# =============================================================================
# STACKED BAR CHART: Average vs Maximum Speed - Cumberland
# =============================================================================

broadband_cumberland_agg <- broadband_agg %>%
  filter(county == "Cumberland") %>%
  pivot_longer(
    cols = c(avg_download_speed, max_download_speed),
    names_to = "speed_type",
    values_to = "speed"
  ) %>%
  mutate(
    speed_type = case_when(
      speed_type == "avg_download_speed" ~ "Average Speed",
      speed_type == "max_download_speed" ~ "Maximum Speed"
    )
  )

plot_2b_cumberland <- ggplot(broadband_cumberland_agg, aes(x = town, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_y_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_fill_manual(values = c("Average Speed" = "#4ECDC4", "Maximum Speed" = "#FFD93D")) +
  labs(
    title = "Broadband Speed by Town - Cumberland",
    subtitle = "Average vs Maximum Download Speed",
    x = "Town/Local Authority",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(base_path, "Charts/broadband_stacked_bar_cumberland.png"),
  plot = plot_2b_cumberland,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: broadband_stacked_bar_cumberland.png\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== Broadband Speed Summary Statistics ===\n")
cat("\nBy County:\n")
broadband_summary <- broadband_data %>%
  group_by(county) %>%
  summarise(
    mean_speed = mean(avg_download_speed, na.rm = TRUE),
    median_speed = median(avg_download_speed, na.rm = TRUE),
    max_speed = max(max_download_speed, na.rm = TRUE),
    n_postcodes = n()
  )
print(broadband_summary)

cat("\nEDA Broadband visualisations complete!\n")