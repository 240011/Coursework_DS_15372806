# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(fmsb)  # For radar chart

# Set working directory path
base_path <- "."

# Load cleaned data
crime_data <- read_csv(file.path(base_path, "cleaned_data/crime_cleaned.csv"),
                       show_col_types = FALSE)
crime_agg <- read_csv(file.path(base_path, "cleaned_data/crime_aggregated.csv"),
                      show_col_types = FALSE)

# Create Charts directory if not exists
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# =============================================================================
# BOXPLOT: Drug Offence Rate - Town-level, Both Counties (2024)
# =============================================================================

drug_offence_town <- crime_data %>%
  filter(crime_type == "Drugs", year == 2024) %>%
  group_by(county, town) %>%
  summarise(
    crime_count = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(town), town != "")

plot_3a <- ggplot(drug_offence_town, aes(x = county, y = crime_count, fill = county)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Drug Offence Rate by Town (2024)",
    subtitle = "Distribution across Cheshire and Cumberland",
    x = "County",
    y = "Number of Drug Offences",
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
  filename = file.path(base_path, "Charts/crime_drug_boxplot.png"),
  plot = plot_3a,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Saved: crime_drug_boxplot.png\n")

# =============================================================================
# RADAR CHART: Vehicle Crime Rate (2024, Specific Month)
# =============================================================================

# Prepare vehicle crime data for radar chart (may 2024)
vehicle_crime_2024 <- crime_agg %>%
  filter(crime_type == "Vehicle crime", year == 2024, month_num == 5)

# Get top towns for each county
vehicle_radar_data <- vehicle_crime_2024 %>%
  group_by(county) %>%
  summarise(
    crime_rate = sum(crime_rate_per_10k, na.rm = TRUE),
    .groups = "drop"
  )

# Create radar chart using ggplot alternative (polar coordinates)
plot_3b <- ggplot(vehicle_radar_data, aes(x = county, y = crime_rate, fill = county)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  coord_polar(theta = "x") +
  scale_fill_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Vehicle Crime Rate (June 2024)",
    subtitle = "Per 10,000 Population",
    x = "",
    y = "Crime Rate per 10,000",
    fill = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5),
    legend.position = "bottom",
    axis.text.y = element_blank(),
    panel.grid = element_line(color = "grey80")
  )

ggsave(
  filename = file.path(base_path, "Charts/crime_vehicle_radar.png"),
  plot = plot_3b,
  width = 8,
  height = 8,
  dpi = 300
)

cat("Saved: crime_vehicle_radar.png\n")

# =============================================================================
#  PIE CHART: Robbery Rate (2024, Specific Month)
# =============================================================================

robbery_2024 <- crime_agg %>%
  filter(crime_type == "Robbery", year == 2024, month_num == 6) %>%
  group_by(county) %>%
  summarise(
    total_crimes = sum(crime_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = total_crimes / sum(total_crimes) * 100,
    label = paste0(county, "\n", round(percentage, 1), "%")
  )

plot_3c <- ggplot(robbery_2024, aes(x = "", y = total_crimes, fill = county)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.8) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  labs(
    title = "Robbery Rate Distribution (June 2024)",
    subtitle = "Proportion by County",
    fill = "County"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(base_path, "Charts/crime_robbery_pie.png"),
  plot = plot_3c,
  width = 8,
  height = 8,
  dpi = 300
)

cat("Saved: crime_robbery_pie.png\n")

# =============================================================================
# LINE GRAPH: Drug Offence Rate per 10,000 People - Both Counties
# =============================================================================

drug_trend <- crime_agg %>%
  filter(crime_type == "Drugs") %>%
  group_by(county, year) %>%
  summarise(
    total_crime_rate = sum(crime_rate_per_10k, na.rm = TRUE),
    .groups = "drop"
  )

plot_3d <- ggplot(drug_trend, aes(x = year, y = total_crime_rate, color = county, group = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  scale_x_continuous(breaks = seq(min(drug_trend$year), max(drug_trend$year), 1)) +
  labs(
    title = "Drug Offence Rate Trend",
    subtitle = "Per 10,000 Population - Cheshire vs Cumberland",
    x = "Year",
    y = "Drug Offence Rate (per 10,000)",
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
  filename = file.path(base_path, "Charts/crime_drug_trend.png"),
  plot = plot_3d,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: crime_drug_trend.png\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== Crime Statistics Summary ===\n")

cat("\nDrug Offences by County (2024):\n")
drug_summary <- crime_data %>%
  filter(crime_type == "Drugs", year == 2024) %>%
  group_by(county) %>%
  summarise(
    total_crimes = n(),
    .groups = "drop"
  )
print(drug_summary)

cat("\nVehicle Crimes by County (2024):\n")
vehicle_summary <- crime_data %>%
  filter(crime_type == "Vehicle crime", year == 2024) %>%
  group_by(county) %>%
  summarise(
    total_crimes = n(),
    .groups = "drop"
  )
print(vehicle_summary)

cat("\nRobbery by County (2024):\n")
robbery_summary <- crime_data %>%
  filter(crime_type == "Robbery", year == 2024) %>%
  group_by(county) %>%
  summarise(
    total_crimes = n(),
    .groups = "drop"
  )
print(robbery_summary)

cat("\nEDA Crime visualisations complete!\n")