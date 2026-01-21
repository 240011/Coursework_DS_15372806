# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(broom)

# Set working directory path
base_path <- "."

# Load cleaned data
house_price_data <- read_csv(file.path(base_path, "cleaned_data/house_price_cleaned.csv"),
                             show_col_types = FALSE)
crime_agg <- read_csv(file.path(base_path, "cleaned_data/crime_aggregated.csv"),
                      show_col_types = FALSE)

# Create Charts directory if not exists
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# Aggregate house prices by county and town
house_price_county <- house_price_data %>%
  group_by(county, town) %>%
  summarise(
    avg_house_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate crime data for drug offences (2023)
drug_crime_2023 <- crime_agg %>%
  filter(crime_type == "Drugs", year == 2023) %>%
  group_by(county, town) %>%
  summarise(
    drug_offence_rate = sum(crime_rate_per_10k, na.rm = TRUE),
    total_drug_crimes = sum(crime_count, na.rm = TRUE),
    .groups = "drop"
  )

# Merge house price with crime data
merged_hp_crime <- house_price_county %>%
  inner_join(drug_crime_2023, by = c("county", "town"))

cat("\n=== Linear Model: House Price vs Drug Offence Rate (2023) ===\n")
cat("Data points:", nrow(merged_hp_crime), "\n")

# Check if we have data
if (nrow(merged_hp_crime) == 0) {
  cat("\nError: No matching data found between house prices and crime data.\n")
  cat("Please check that town names match between datasets.\n")
  stop("No data to analyze")
}

# Fit linear model: House Price ~ Drug Offence Rate
model_hp_crime <- lm(avg_house_price ~ drug_offence_rate, data = merged_hp_crime)

# Print model summary
cat("\nModel Summary:\n")
print(summary(model_hp_crime))

# Get tidy model results
model_results <- tidy(model_hp_crime)
cat("\nCoefficients:\n")
print(model_results)

# Calculate R-squared
r_squared <- glance(model_hp_crime)$r.squared
cat("\nR-squared:", round(r_squared, 4), "\n")

# Get confidence intervals
cat("\n95% Confidence Intervals:\n")
print(confint(model_hp_crime))

# Residuals analysis
cat("\nResiduals Summary:\n")
print(summary(model_hp_crime$residuals))

# Scatter plot with regression line
plot_hp_crime <- ggplot(merged_hp_crime, aes(x = drug_offence_rate, y = avg_house_price, color = county)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", alpha = 0.3) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "House Price vs Drug Offence Rate (2023)",
    subtitle = paste0("Linear Regression (R² = ", round(r_squared, 3), ")"),
    x = "Drug Offence Rate (per 10,000 population)",
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

# Add regression equation if coefficients exist
if (!is.na(coef(model_hp_crime)[2])) {
  # Calculate position for annotation
  x_pos <- max(merged_hp_crime$drug_offence_rate, na.rm = TRUE) * 0.7
  y_pos <- max(merged_hp_crime$avg_house_price, na.rm = TRUE) * 0.9
  
  # Format the equation
  intercept <- round(coef(model_hp_crime)[1], 0)
  slope <- round(coef(model_hp_crime)[2], 0)
  
  # Handle positive/negative slope in equation
  slope_sign <- ifelse(slope >= 0, "+", "")
  
  plot_hp_crime <- plot_hp_crime +
    annotate("text", 
             x = x_pos,
             y = y_pos,
             label = paste0("y = ", format(intercept, big.mark = ","), " ", 
                            slope_sign, " ", format(slope, big.mark = ","), "x"),
             size = 4, fontface = "italic", color = "darkgray")
}

# Save plot
ggsave(
  filename = file.path(base_path, "Charts/linear_model_hp_crime.png"),
  plot = plot_hp_crime,
  width = 10,
  height = 7,
  dpi = 300
)

cat("\nSaved: linear_model_hp_crime.png\n")
cat("\nLinear model analysis complete!\n")