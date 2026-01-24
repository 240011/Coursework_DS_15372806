# =============================================================================
# Load Required Libraries
# =============================================================================
library(tidyverse)
library(ggplot2)
library(scales)
library(broom)

# =============================================================================
# Set Working Directory
# =============================================================================
base_path <- "."

# =============================================================================
# Load Cleaned Data
# =============================================================================
house_price_data <- read_csv(
  file.path(base_path, "cleaned_data/house_price_cleaned.csv"),
  show_col_types = FALSE
)

crime_agg <- read_csv(
  file.path(base_path, "cleaned_data/crime_aggregated.csv"),
  show_col_types = FALSE
)

# =============================================================================
# Create Charts Directory (if not exists)
# =============================================================================
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# =============================================================================
# Aggregate House Prices by County & Town
# =============================================================================
house_price_county <- house_price_data %>%
  group_by(county, town) %>%
  summarise(
    avg_house_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# Aggregate Vehicle Crime Data (2023)
# =============================================================================
vehicle_crime_2023 <- crime_agg %>%
  filter(crime_type == "Vehicle crime", year == 2023) %>%
  group_by(county, town) %>%
  summarise(
    vehicle_crime_rate = sum(crime_rate_per_10k, na.rm = TRUE),
    total_vehicle_crimes = sum(crime_count, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# Merge House Price and Vehicle Crime Data
# =============================================================================
merged_hp_vehicle <- house_price_county %>%
  inner_join(vehicle_crime_2023, by = c("county", "town"))

cat("\n=== Linear Model: House Price vs Vehicle Crime Rate (2023) ===\n")
cat("Data points:", nrow(merged_hp_vehicle), "\n")

# Stop if no data
if (nrow(merged_hp_vehicle) == 0) {
  stop("No matching data found. Check town names across datasets.")
}

# =============================================================================
# Fit Linear Regression Model
# =============================================================================
model_hp_vehicle <- lm(avg_house_price ~ vehicle_crime_rate,
                       data = merged_hp_vehicle)

# =============================================================================
# Model Output
# =============================================================================
cat("\nModel Summary:\n")
print(summary(model_hp_vehicle))

cat("\nCoefficients:\n")
print(tidy(model_hp_vehicle))

r_squared <- glance(model_hp_vehicle)$r.squared
cat("\nR-squared:", round(r_squared, 4), "\n")

cat("\n95% Confidence Intervals:\n")
print(confint(model_hp_vehicle))

cat("\nResiduals Summary:\n")
print(summary(model_hp_vehicle$residuals))

# =============================================================================
# Scatter Plot with Regression Line
# =============================================================================
plot_hp_vehicle <- ggplot(
  merged_hp_vehicle,
  aes(x = vehicle_crime_rate,
      y = avg_house_price,
      color = county)
) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm",
              se = TRUE,
              linetype = "dashed",
              color = "black",
              alpha = 0.3) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_color_manual(values = c(
    "Cheshire" = "#FF6B6B",
    "Cumberland" = "#4ECDC4"
  )) +
  labs(
    title = "House Price vs Vehicle Crime Rate (2023)",
    subtitle = paste0("Linear Regression (R² = ", round(r_squared, 3), ")"),
    x = "Vehicle Crime Rate (per 10,000 population)",
    y = "Average House Price (£)",
    color = "County",
    caption = "Data: UK Land Registry & UK Police Crime Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# =============================================================================
# Add Regression Equation Annotation
# =============================================================================
intercept <- round(coef(model_hp_vehicle)[1], 0)
slope <- round(coef(model_hp_vehicle)[2], 0)
sign <- ifelse(slope >= 0, "+", "")

plot_hp_vehicle <- plot_hp_vehicle +
  annotate(
    "text",
    x = max(merged_hp_vehicle$vehicle_crime_rate, na.rm = TRUE) * 0.7,
    y = max(merged_hp_vehicle$avg_house_price, na.rm = TRUE) * 0.9,
    label = paste0(
      "y = ",
      format(intercept, big.mark = ","),
      " ",
      sign,
      format(slope, big.mark = ","),
      "x"
    ),
    size = 4,
    fontface = "italic",
    color = "darkgray"
  )

# =============================================================================
# Save Plot
# =============================================================================
ggsave(
  filename = file.path(base_path, "Charts/linear_model_hp_vehicle_crime.png"),
  plot = plot_hp_vehicle,
  width = 10,
  height = 7,
  dpi = 300
)

cat("\nSaved: linear_model_hp_vehicle_crime.png\n")
cat("\nLinear model analysis complete!\n")

