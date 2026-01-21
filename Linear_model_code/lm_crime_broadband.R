# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(broom)

# Set working directory path
base_path <- "."

# Load cleaned data
broadband_agg <- read_csv(file.path(base_path, "cleaned_data/broadband_aggregated.csv"),
                          show_col_types = FALSE)
crime_agg <- read_csv(file.path(base_path, "cleaned_data/crime_aggregated.csv"),
                      show_col_types = FALSE)

# Create Charts directory if not exists
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# Aggregate crime data for drug offences
drug_crime <- crime_agg %>%
  filter(crime_type == "Drugs") %>%
  group_by(county, town) %>%
  summarise(
    drug_offence_rate = mean(crime_rate_per_10k, na.rm = TRUE),
    total_drug_crimes = sum(crime_count, na.rm = TRUE),
    .groups = "drop"
  )

# Merge broadband with crime data
merged_broadband_crime <- broadband_agg %>%
  inner_join(drug_crime, by = c("county", "town"))

cat("\n=== Linear Model: Download Speed vs Drug Offence Rate ===\n")
cat("Data points:", nrow(merged_broadband_crime), "\n")

# Check if we have data
if (nrow(merged_broadband_crime) == 0) {
  cat("\nError: No matching data found between broadband and crime data.\n")
  stop("No data to analyze")
}

# Fit linear model: Download Speed ~ Drug Offence Rate
model_broadband_crime <- lm(avg_download_speed ~ drug_offence_rate, data = merged_broadband_crime)

# Print model summary
cat("\nModel Summary:\n")
print(summary(model_broadband_crime))

# Get tidy model results
model_results <- tidy(model_broadband_crime)
cat("\nCoefficients:\n")
print(model_results)

# Calculate R-squared
r_squared <- glance(model_broadband_crime)$r.squared
cat("\nR-squared:", round(r_squared, 4), "\n")

# Get confidence intervals
cat("\n95% Confidence Intervals:\n")
print(confint(model_broadband_crime))

# Residuals analysis
cat("\nResiduals Summary:\n")
print(summary(model_broadband_crime$residuals))

# Scatter plot with regression line
plot_broadband_crime <- ggplot(merged_broadband_crime, aes(x = drug_offence_rate, y = avg_download_speed, color = county)) +
  geom_point(alpha = 0.8, size = 4) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 1.2, color = "#2C3E50", alpha = 0.2) +
  scale_y_continuous(labels = label_number(suffix = " Mbit/s")) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Download Speed vs Drug Offence Rate",
    subtitle = paste0("Linear Regression (R² = ", round(r_squared, 3), ")"),
    x = "Drug Offence Rate (per 10,000 population)",
    y = "Average Download Speed (Mbit/s)",
    color = "County",
    caption = "Data: Ofcom Broadband Data & UK Police Crime Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

# Add regression equation if coefficients exist
if (!is.na(coef(model_broadband_crime)[2])) {
  # Calculate position for annotation
  x_pos <- max(merged_broadband_crime$drug_offence_rate, na.rm = TRUE) * 0.7
  y_pos <- max(merged_broadband_crime$avg_download_speed, na.rm = TRUE) * 0.9
  
  # Format the equation
  intercept <- round(coef(model_broadband_crime)[1], 2)
  slope <- round(coef(model_broadband_crime)[2], 4)
  
  # Handle positive/negative slope in equation
  slope_sign <- ifelse(slope >= 0, "+", "")
  
  plot_broadband_crime <- plot_broadband_crime +
    annotate("text", 
             x = x_pos,
             y = y_pos,
             label = paste0("y = ", format(intercept, big.mark = ","), " ", 
                            slope_sign, " ", slope, "x"),
             size = 5, fontface = "bold.italic", color = "#2C3E50")
}

# Save plot
ggsave(
  filename = file.path(base_path, "Charts/linear_model_broadband_crime.png"),
  plot = plot_broadband_crime,
  width = 12,
  height = 8,
  dpi = 300
)

cat("\nSaved: linear_model_broadband_crime.png\n")
cat("\nLinear model analysis complete!\n")