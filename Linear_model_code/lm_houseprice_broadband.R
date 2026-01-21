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
broadband_data <- read_csv(file.path(base_path, "cleaned_data/broadband_cleaned.csv"),
                           show_col_types = FALSE)

# Create Charts directory if not exists
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# =============================================================================
# AGGREGATE DATA AT COUNTY LEVEL (to ensure matching)
# =============================================================================

# Aggregate house prices by county only
house_price_by_county <- house_price_data %>%
  group_by(county) %>%
  summarise(
    avg_house_price = mean(price, na.rm = TRUE),
    median_house_price = median(price, na.rm = TRUE),
    n_transactions = n(),
    .groups = "drop"
  )

# Aggregate broadband by county only
broadband_by_county <- broadband_data %>%
  group_by(county) %>%
  summarise(
    avg_download_speed = mean(avg_download_speed, na.rm = TRUE),
    max_download_speed = max(max_download_speed, na.rm = TRUE),
    n_postcodes = n(),
    .groups = "drop"
  )

# Merge at county level
merged_hp_broadband <- house_price_by_county %>%
  inner_join(broadband_by_county, by = "county")

cat("\n=== Linear Model: House Price vs Download Speed ===\n")
cat("Counties in merged data:\n")
print(merged_hp_broadband$county)
cat("Data points:", nrow(merged_hp_broadband), "\n")

# Check if we have data
if (nrow(merged_hp_broadband) == 0) {
  cat("\nError: No matching data found between house prices and broadband data.\n")
  stop("No data to analyze")
}

# Fit linear model: House Price ~ Download Speed
model_hp_broadband <- lm(avg_house_price ~ avg_download_speed, data = merged_hp_broadband)

# Print model summary
cat("\nModel Summary:\n")
print(summary(model_hp_broadband))

# Get tidy model results
model_results <- tidy(model_hp_broadband)
cat("\nCoefficients:\n")
print(model_results)

# Calculate R-squared
r_squared <- glance(model_hp_broadband)$r.squared
cat("\nR-squared:", round(r_squared, 4), "\n")

# Get confidence intervals
cat("\n95% Confidence Intervals:\n")
print(confint(model_hp_broadband))

# Residuals analysis
cat("\nResiduals Summary:\n")
print(summary(model_hp_broadband$residuals))

# Scatter plot with regression line
plot_hp_broadband <- ggplot(merged_hp_broadband, aes(x = avg_download_speed, y = avg_house_price, color = county)) +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", alpha = 0.3) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_x_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "House Price vs Download Speed",
    subtitle = paste0("Linear Regression (R² = ", round(r_squared, 3), ")"),
    x = "Average Download Speed (Mbit/s)",
    y = "Average House Price (£)",
    color = "County",
    caption = "Data: UK Land Registry & Ofcom Broadband Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Add regression equation if coefficients exist
if (!is.na(coef(model_hp_broadband)[2])) {
  # Calculate position for annotation
  x_pos <- max(merged_hp_broadband$avg_download_speed, na.rm = TRUE) * 0.8
  y_pos <- max(merged_hp_broadband$avg_house_price, na.rm = TRUE) * 0.9
  
  # Format the equation
  intercept <- round(coef(model_hp_broadband)[1], 0)
  slope <- round(coef(model_hp_broadband)[2], 0)
  
  # Handle positive/negative slope in equation
  slope_sign <- ifelse(slope >= 0, "+", "")
  
  plot_hp_broadband <- plot_hp_broadband +
    annotate("text", 
             x = x_pos,
             y = y_pos,
             label = paste0("y = ", format(intercept, big.mark = ","), " ", 
                            slope_sign, " ", format(slope, big.mark = ","), "x"),
             size = 3.5, fontface = "italic", color = "darkgray")
}

# Save plot
ggsave(
  filename = file.path(base_path, "Charts/linear_model_hp_broadband.png"),
  plot = plot_hp_broadband,
  width = 10,
  height = 7,
  dpi = 300
)

cat("\nSaved: linear_model_hp_broadband.png\n")
cat("\nLinear model analysis complete!\n