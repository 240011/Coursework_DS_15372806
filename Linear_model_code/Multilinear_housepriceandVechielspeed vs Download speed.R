library(tidyverse)

base_path <- "."

# ============================================================
# Load data
# ============================================================
data <- read_csv(
  file.path(base_path, "cleaned_data/merged_dataset.csv"),
  show_col_types = FALSE
)

if (!dir.exists(file.path(base_path, "Charts"))) {
  dir.create(file.path(base_path, "Charts"))
}

# ============================================================
# Standardise column names
# ============================================================
names(data) <- tolower(names(data))

price_col   <- intersect(names(data), c("avg_house_price", "mean_price", "price", "avg_price"))[1]
speed_col   <- intersect(names(data), c("avg_download_speed", "download_speed", "speed"))[1]
vehicle_col <- intersect(names(data), c("vehicle_crime", "vehicle_crimes", "vehicle_crime_rate"))[1]
county_col  <- intersect(names(data), c("county", "region"))[1]
year_col    <- intersect(names(data), c("year"))[1]

if (any(is.na(c(price_col, speed_col, vehicle_col, county_col, year_col)))) {
  stop("❌ Required columns missing in merged_dataset.csv")
}

# ============================================================
# Clean data
# ============================================================
data_clean <- data %>%
  filter(
    !is.na(.data[[price_col]]),
    !is.na(.data[[speed_col]]),
    !is.na(.data[[vehicle_col]]),
    !is.na(.data[[county_col]]),
    !is.na(.data[[year_col]])
  )

# ============================================================
# County-year aggregation
# ============================================================
county_data <- data_clean %>%
  group_by(
    county = .data[[county_col]],
    year   = .data[[year_col]]
  ) %>%
  summarise(
    avg_house_price    = mean(.data[[price_col]], na.rm = TRUE),
    avg_download_speed = mean(.data[[speed_col]], na.rm = TRUE),
    vehicle_crime_rate = mean(.data[[vehicle_col]], na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# MULTIVARIATE GRAPH
# ============================================================
p <- ggplot(county_data,
            aes(x = avg_download_speed,
                y = avg_house_price,
                color = vehicle_crime_rate,
                size = vehicle_crime_rate)) +
  geom_point(alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(county ~ year) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(3, 8)) +
  labs(
    title = "House Price & Vehicle Crime Rate vs Download Speed",
    subtitle = "Color & size represent vehicle crime rate",
    x = "Download Speed (Mbps)",
    y = "Average House Price (£)",
    color = "Vehicle Crime Rate",
    size = "Vehicle Crime Rate"
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(base_path, "Charts/multivariate_houseprice_vehiclecrime_downloadspeed.png"),
       p, width = 12, height = 7)

print(p)

cat("\n✅ Multivariate graph saved in Charts folder\n")
