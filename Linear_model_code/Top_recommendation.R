# =============================================================================
# Town Recommendation System: Top 10 Towns Based on Overall Score
# Housing affordability + Internet quality + Crime safety
# =============================================================================

library(dplyr)

base_path <- "."

# =============================================================================
# Load datasets
# =============================================================================
data <- read.csv(file.path(base_path, "cleaned_data/merged_dataset.csv"),
                 stringsAsFactors = FALSE)

population_data <- read.csv(file.path(base_path, "cleaned_data/population_cleaned.csv"),
                            stringsAsFactors = FALSE)

# Create folders if missing
if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}
if (!dir.exists(file.path(base_path, "linear_modelling"))) {
  dir.create(file.path(base_path, "linear_modelling"))
}

# =============================================================================
# Calculate average population per postcode
# =============================================================================
avg_pop_per_postcode <- population_data %>%
  filter(!is.na(population_2024)) %>%
  summarise(avg_pop = mean(population_2024, na.rm = TRUE)) %>%
  pull(avg_pop)

# =============================================================================
# Filter 2024 data & compute metrics
# =============================================================================
town_metrics <- data %>%
  filter(year == 2024) %>%
  filter(!is.na(avg_house_price),
         !is.na(avg_download_speed),
         !is.na(drug_offence),
         !is.na(vehicle_crime),
         !is.na(postcode_count)) %>%
  mutate(
    drug_offence_rate   = drug_offence / (postcode_count * avg_pop_per_postcode) * 10000,
    vehicle_crime_rate  = vehicle_crime / (postcode_count * avg_pop_per_postcode) * 10000,
    total_crime_rate    = drug_offence_rate + vehicle_crime_rate
  ) %>%
  select(county, town, avg_house_price, avg_download_speed,
         drug_offence_rate, vehicle_crime_rate, total_crime_rate, postcode_count)

cat("Dataset Overview for 2024:\n")
cat("Total towns analyzed:", nrow(town_metrics), "\n")
cat("Cheshire towns:", nrow(filter(town_metrics, county == "Cheshire")), "\n")
cat("Cumberland towns:", nrow(filter(town_metrics, county == "Cumberland")), "\n\n")

# =============================================================================
# Percentile benchmarks
# =============================================================================
percentiles <- town_metrics %>%
  summarise(
    house_price_25th = quantile(avg_house_price, 0.25, na.rm = TRUE),
    house_price_50th = quantile(avg_house_price, 0.50, na.rm = TRUE),
    house_price_75th = quantile(avg_house_price, 0.75, na.rm = TRUE),
    house_price_max  = max(avg_house_price, na.rm = TRUE),
    
    download_speed_25th = quantile(avg_download_speed, 0.25, na.rm = TRUE),
    download_speed_50th = quantile(avg_download_speed, 0.50, na.rm = TRUE),
    download_speed_75th = quantile(avg_download_speed, 0.75, na.rm = TRUE),
    download_speed_max  = max(avg_download_speed, na.rm = TRUE),
    
    crime_rate_25th = quantile(total_crime_rate, 0.25, na.rm = TRUE),
    crime_rate_50th = quantile(total_crime_rate, 0.50, na.rm = TRUE),
    crime_rate_75th = quantile(total_crime_rate, 0.75, na.rm = TRUE),
    crime_rate_max  = max(total_crime_rate, na.rm = TRUE)
  )

cat("Percentile Benchmarks:\n")
print(percentiles)

# =============================================================================
# Calculate scores
# =============================================================================
town_scores <- town_metrics %>%
  mutate(
    housing_score = case_when(
      avg_house_price <= percentiles$house_price_25th ~ 100,
      avg_house_price <= percentiles$house_price_50th ~ 75,
      avg_house_price <= percentiles$house_price_75th ~ 50,
      avg_house_price <= percentiles$house_price_max  ~ 25,
      TRUE ~ 0
    ),
    
    internet_score = case_when(
      avg_download_speed >= percentiles$download_speed_75th ~ 100,
      avg_download_speed >= percentiles$download_speed_50th ~ 75,
      avg_download_speed >= percentiles$download_speed_25th ~ 50,
      avg_download_speed > 0 ~ 25,
      TRUE ~ 0
    ),
    
    crime_score = case_when(
      total_crime_rate <= percentiles$crime_rate_25th ~ 100,
      total_crime_rate <= percentiles$crime_rate_50th ~ 75,
      total_crime_rate <= percentiles$crime_rate_75th ~ 50,
      total_crime_rate <= percentiles$crime_rate_max  ~ 25,
      TRUE ~ 0
    )
  )

# =============================================================================
# Overall weighted score
# =============================================================================
town_scores <- town_scores %>%
  mutate(
    overall_score = (housing_score * 0.40) +
      (internet_score * 0.35) +
      (crime_score * 0.25),
    rank = rank(-overall_score)
  ) %>%
  arrange(rank)

top_10_towns <- head(town_scores, 10)

# =============================================================================
# Print results
# =============================================================================
cat("\n================================================================================\n")
cat("TOP 10 TOWNS RECOMMENDATION - 2024\n")
cat("================================================================================\n\n")

for(i in 1:nrow(top_10_towns)) {
  town <- top_10_towns[i, ]
  cat(sprintf("#%d %s, %s\n", as.integer(town$rank), town$town, town$county))
  cat(sprintf("Overall Score: %.1f/100\n", town$overall_score))
  cat(sprintf("  Housing Score: %.0f/100 (Price: £%s)\n",
              town$housing_score,
              format(round(town$avg_house_price, 0), big.mark = ",")))
  cat(sprintf("  Internet Score: %.0f/100 (Speed: %.1f Mbps)\n",
              town$internet_score, town$avg_download_speed))
  cat(sprintf("  Crime Score: %.0f/100 (Rate: %.4f per 10,000)\n",
              town$crime_score, town$total_crime_rate))
  cat("------------------------------------------------------------\n")
}

# =============================================================================
# Plot: Top 10 towns
# =============================================================================
png(file.path(base_path, "Charts/top_10_towns_recommendation.png"),
    width = 1200, height = 800)

par(mar = c(5, 8, 4, 2))

barplot(top_10_towns$overall_score,
        names.arg = top_10_towns$town,
        horiz = TRUE,
        las = 1,
        col = ifelse(top_10_towns$county == "Cheshire", "#FF6B6B", "#4ECDC4"),
        main = "Top 10 Towns Recommendation (2024)",
        xlab = "Overall Score",
        xlim = c(0, 100),
        cex.names = 1.1,
        cex.main = 1.4)

grid(col = "lightgray", lty = 2)

legend("bottomright",
       legend = c("Cheshire", "Cumberland"),
       col = c("#FF6B6B", "#4ECDC4"),
       pch = 15,
       cex = 1.2)

dev.off()

# =============================================================================
# Save CSV
# =============================================================================
results <- town_scores %>%
  select(rank, county, town, overall_score,
         housing_score, internet_score, crime_score,
         avg_house_price, avg_download_speed, total_crime_rate) %>%
  arrange(rank)

write.csv(results,
          file.path(base_path, "linear_modelling/top_10_towns_recommendation.csv"),
          row.names = FALSE)

# =============================================================================
# Summary stats
# =============================================================================
cat("\n================================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("================================================================================\n")

cat("Top 10 Counties Distribution:\n")
print(top_10_towns %>% count(county))

cat("\nAverage Scores in Top 10:\n")
print(top_10_towns %>%
        summarise(
          avg_overall = mean(overall_score),
          avg_housing = mean(housing_score),
          avg_internet = mean(internet_score),
          avg_crime = mean(crime_score)
        ))

cat("\n✅ Analysis complete!\n")
cat("📁 Saved:\n")
cat("- Charts/top_10_towns_recommendation.png\n")
cat("- linear_modelling/top_10_towns_recommendation.csv\n")
