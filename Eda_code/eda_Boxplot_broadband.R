# =============================================================================
# BOXPLOT: Average Download Speed - Both Counties
# =============================================================================

library(tidyverse)
library(scales)
# Set working directory path
base_path <- "."

# Load cleaned data
broadband_data <- read_csv(file.path(base_path, "cleaned_data/broadband_cleaned.csv"),
                           show_col_types = FALSE)
broadband_agg <- read_csv(file.path(base_path, "cleaned_data/broadband_aggregated.csv"),
                          show_col_types = FALSE)


broadband_both <- broadband_data %>%
  filter(county %in% c("Cheshire", "Cumberland"))

# Create combined colour palette for all towns
n_towns <- length(unique(broadband_both$town))
town_colors <- colorRampPalette(c("#FF6B6B", "#FFA07A", "#FFB6C1",
                                  "#4ECDC4", "#45B7D1", "#96CEB4"))(n_towns)

plot_broadband_both <- ggplot(broadband_both, aes(x = town, y = avg_download_speed, fill = town)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~ county, scales = "free_x") +
  scale_y_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_fill_manual(values = town_colors) +
  labs(
    title = "Average Download Speed Distribution by Town",
    subtitle = "Cheshire vs Cumberland",
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
  filename = file.path(base_path, "Charts/broadband_boxplot_both_counties.png"),
  plot = plot_broadband_both,
  width = 12,
  height = 6,
  dpi = 300
)

cat("Saved: broadband_boxplot_both_counties.png\n")
