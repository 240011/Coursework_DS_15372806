
# County-Level Multivariate Regression:
# House Price vs Drug Offence Rate + Vehicle Crime Rate
# Comparing Cheshire vs Cumberland
# =============================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(broom)
library(patchwork)

base_path <- "."

# =============================================================================
# Load cleaned data
# =============================================================================
house_price_agg <- read_csv(
  file.path(base_path, "cleaned_data/house_price_aggregated.csv"),
  show_col_types = FALSE
)

crime_agg <- read_csv(
  file.path(base_path, "cleaned_data/crime_aggregated.csv"),
  show_col_types = FALSE
)

if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# =============================================================================
# Detect house price column safely
# =============================================================================
price_col <- intersect(names(house_price_agg),
                       c("avg_price", "price", "avg_house_price", "mean_price"))[1]

if (is.na(price_col)) stop("No house price column found in house_price_aggregated.csv")

# =============================================================================
# Aggregate drug + vehicle crime (county-year)
# =============================================================================
crime_county <- crime_agg %>%
  filter(crime_type %in% c("Drugs", "Vehicle crime")) %>%
  group_by(county, year, crime_type) %>%
  summarise(
    crime_rate = mean(crime_rate_per_10k, na.rm = TRUE),
    crime_count = sum(crime_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = crime_type,
    values_from = c(crime_rate, crime_count),
    names_glue = "{.value}_{crime_type}"
  )

# =============================================================================
# Aggregate house prices (county-year)
# =============================================================================
house_price_county <- house_price_agg %>%
  group_by(county, year) %>%
  summarise(
    avg_house_price = mean(.data[[price_col]], na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# Merge datasets
# =============================================================================
merged_hp_crime <- house_price_county %>%
  inner_join(crime_county, by = c("county", "year")) %>%
  rename(
    drug_offence_rate   = `crime_rate_Drugs`,
    vehicle_crime_rate = `crime_rate_Vehicle crime`,
    total_drug_offences   = `crime_count_Drugs`,
    total_vehicle_crime = `crime_count_Vehicle crime`
  )

cat("\n=== Multivariate Linear Model: House Price vs Drug + Vehicle Crime ===\n")
cat("Data points:", nrow(merged_hp_crime), "\n")

if (nrow(merged_hp_crime) == 0) stop("No matching data after merge.")

# =============================================================================
# Correlations
# =============================================================================
cat("\nCorrelations:\n")
cat("House Price vs Drug Crime:",
    cor(merged_hp_crime$avg_house_price, merged_hp_crime$drug_offence_rate), "\n")
cat("House Price vs Vehicle Crime:",
    cor(merged_hp_crime$avg_house_price, merged_hp_crime$vehicle_crime_rate), "\n")
cat("Drug Crime vs Vehicle Crime:",
    cor(merged_hp_crime$drug_offence_rate, merged_hp_crime$vehicle_crime_rate), "\n")

# =============================================================================
# Multivariate Regression
# =============================================================================
multi_model <- lm(avg_house_price ~ drug_offence_rate + vehicle_crime_rate,
                  data = merged_hp_crime)

cat("\nMultivariate Regression Results:\n")
print(summary(multi_model))
print(tidy(multi_model))

r2 <- glance(multi_model)$r.squared
cat("\nR-squared:", round(r2, 4), "\n")

# =============================================================================
# Scatter plots
# =============================================================================
plot_drug <- ggplot(
  merged_hp_crime,
  aes(x = drug_offence_rate,
      y = avg_house_price,
      color = county)
) +
  geom_point(size = 3, alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE,
              linetype = "dashed", color = "#2C3E50", alpha = 0.25) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B",
                                "Cumberland" = "#4ECDC4")) +
  labs(
    title = "House Price vs Drug Offence Rate",
    x = "Drug Offence Rate (per 10,000 population)",
    y = "Average House Price (£)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot_vehicle <- ggplot(
  merged_hp_crime,
  aes(x = vehicle_crime_rate,
      y = avg_house_price,
      color = county)
) +
  geom_point(size = 3, alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE,
              linetype = "dashed", color = "#2C3E50", alpha = 0.25) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B",
                                "Cumberland" = "#4ECDC4")) +
  labs(
    title = "House Price vs Vehicle Crime Rate",
    x = "Vehicle Crime Rate (per 10,000 population)",
    y = "Average House Price (£)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

combined_plot <- plot_drug + plot_vehicle

ggsave(
  filename = file.path(base_path, "Charts/multivariate_house_price_vs_crime_rates.png"),
  plot = combined_plot,
  width = 14,
  height = 6,
  dpi = 300
)

cat("\nSaved: multivariate_house_price_vs_crime_rates.png\n")

# =============================================================================
# Time series plots
# =============================================================================
plot_ts_drug <- ggplot(
  merged_hp_crime,
  aes(x = year, y = drug_offence_rate, color = county)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B",
                                "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Drug Offence Rate by Year",
    x = "Year",
    y = "Drug Offence Rate (per 10,000 population)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot_ts_vehicle <- ggplot(
  merged_hp_crime,
  aes(x = year, y = vehicle_crime_rate, color = county)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B",
                                "Cumberland" = "#4ECDC4")) +
  labs(
    title = "Vehicle Crime Rate by Year",
    x = "Year",
    y = "Vehicle Crime Rate (per 10,000 population)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(base_path, "Charts/crime_rates_time_series.png"),
  plot = plot_ts_drug + plot_ts_vehicle,
  width = 14,
  height = 6,
  dpi = 300
)

cat("Saved: crime_rates_time_series.png\n")

# =============================================================================
# Save results
# =============================================================================
results <- merged_hp_crime %>%
  select(county, year, avg_house_price,
         drug_offence_rate, vehicle_crime_rate,
         total_drug_offences, total_vehicle_crime)

write_csv(results,
          file.path(base_path, "Charts/house_price_drug_vehicle_crime_results.csv"))

cat("Saved: house_price_drug_vehicle_crime_results.csv\n")
cat("\nMultivariate analysis complete!\n")
