# =============================================================================
# DATA CLEANING SCRIPT
# ST5014CEM - Data Science for Developers
# Counties: Cheshire and Cumberland
# =============================================================================

# Load required libraries
library(tidyverse)
library(lubridate)

# Set base path
base_path <- "."

# =============================================================================
# 1. LOAD AND CLEAN HOUSE PRICE DATA
# =============================================================================

clean_house_price_data <- function() {
  cat("Loading house price data...\n")
  
  col_names <- c(
    "transaction_id", "price", "date_of_transfer", "postcode",
    "property_type", "old_new", "duration", "paon", "saon",
    "street", "locality", "town", "district", "county",
    "ppd_cat", "record_status"
  )
  
  hp_2022 <- read_csv(file.path(base_path, "Raw data/house_price/2022/pp-2022.csv"),
                      col_names = col_names, show_col_types = FALSE)
  hp_2023 <- read_csv(file.path(base_path, "Raw data/house_price/2023/pp-2023.csv"),
                      col_names = col_names, show_col_types = FALSE)
  hp_2024 <- read_csv(file.path(base_path, "Raw data/house_price/2024/pp-2024.csv"),
                      col_names = col_names, show_col_types = FALSE)
  
  house_price_all <- bind_rows(
    hp_2022 %>% mutate(year = 2022),
    hp_2023 %>% mutate(year = 2023),
    hp_2024 %>% mutate(year = 2024)
  )
  
  cheshire_districts <- c("CHESHIRE EAST", "CHESHIRE WEST AND CHESTER")
  cumberland_districts <- c("ALLERDALE", "CARLISLE", "COPELAND", "CUMBERLAND")
  
  house_price_clean <- house_price_all %>%
    mutate(
      district = toupper(district),
      town = str_to_title(toupper(town))
    ) %>%
    filter(district %in% c(cheshire_districts, cumberland_districts)) %>%
    mutate(
      county = case_when(
        district %in% cheshire_districts ~ "Cheshire",
        district %in% cumberland_districts ~ "Cumberland"
      )
    ) %>%
    select(transaction_id, price, date_of_transfer, postcode,
           town, district, county, year) %>%
    filter(!is.na(price), price > 0)
  
  cat("House price cleaned:", nrow(house_price_clean), "rows\n")
  return(house_price_clean)
}

# =============================================================================
# 2. LOAD AND CLEAN BROADBAND DATA
# =============================================================================

clean_broadband_data <- function() {
  cat("Loading broadband data...\n")
  
  broadband_raw <- read_csv(
    file.path(base_path, "Raw data/broadband_speed/201805_fixed_pc_performance_r03.csv"),
    show_col_types = FALSE
  )
  
  postcode_lsoa <- read_csv(
    file.path(base_path, "Raw data/LSOA/postcode_to_lsoa.csv"),
    show_col_types = FALSE
  )
  
  broadband_clean <- broadband_raw %>%
    rename(
      postcode = 1,
      avg_download_speed = 5,
      max_download_speed = 7
    ) %>%
    select(postcode, avg_download_speed, max_download_speed) %>%
    filter(!is.na(avg_download_speed))
  
  postcode_lsoa_clean <- postcode_lsoa %>%
    select(pcds, ladnm) %>%
    rename(postcode = pcds, local_authority = ladnm) %>%
    mutate(postcode = str_replace_all(postcode, " ", ""))
  
  cheshire_la <- c("Cheshire East", "Cheshire West and Chester")
  cumberland_la <- c("Allerdale", "Carlisle", "Copeland", "Cumberland")
  
  broadband_final <- broadband_clean %>%
    mutate(postcode_join = str_replace_all(postcode, " ", "")) %>%
    left_join(postcode_lsoa_clean, by = c("postcode_join" = "postcode")) %>%
    filter(local_authority %in% c(cheshire_la, cumberland_la)) %>%
    mutate(
      county = case_when(
        local_authority %in% cheshire_la ~ "Cheshire",
        local_authority %in% cumberland_la ~ "Cumberland"
      ),
      town = local_authority
    ) %>%
    select(postcode, town, county, avg_download_speed, max_download_speed)
  
  cat("Broadband cleaned:", nrow(broadband_final), "rows\n")
  return(broadband_final)
}

# =============================================================================
# 3. LOAD AND CLEAN CRIME DATA
# =============================================================================

clean_crime_data <- function() {
  cat("Loading crime data...\n")
  
  crime_dirs <- list.dirs(file.path(base_path, "Raw Data/crime"), recursive = FALSE)
  
  crime_all <- map_dfr(crime_dirs, function(dir) {
    files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    map_dfr(files, function(f) {
      read_csv(f, show_col_types = FALSE) %>%
        mutate(
          county = case_when(
            grepl("cheshire", f, ignore.case = TRUE) ~ "Cheshire",
            grepl("cumbria", f, ignore.case = TRUE) ~ "Cumberland"
          )
        )
    })
  })
  
  crime_clean <- crime_all %>%
    select(
      crime_id = `Crime ID`,
      month = Month,
      crime_type = `Crime type`,
      lsoa_name = `LSOA name`,
      county
    ) %>%
    filter(crime_type %in% c("Drugs", "Vehicle crime", "Robbery")) %>%
    mutate(
      year = as.integer(substr(month, 1, 4)),
      month_num = as.integer(substr(month, 6, 7)),
      town = str_trim(str_extract(lsoa_name, "^[^0-9]+"))
    )
  
  cat("Crime cleaned:", nrow(crime_clean), "rows\n")
  return(crime_clean)
}

# =============================================================================
# 4. AGGREGATE DATA
# =============================================================================

aggregate_house_prices <- function(data) {
  data %>%
    group_by(county, town, year) %>%
    summarise(
      avg_house_price = mean(price, na.rm = TRUE),
      median_house_price = median(price, na.rm = TRUE),
      n_transactions = n(),
      .groups = "drop"
    )
}

aggregate_broadband <- function(data) {
  data %>%
    group_by(county, town) %>%
    summarise(
      avg_download_speed = mean(avg_download_speed, na.rm = TRUE),
      max_download_speed = max(max_download_speed, na.rm = TRUE),
      .groups = "drop"
    )
}

aggregate_crime <- function(data) {
  county_pop <- tibble(
    county = c("Cheshire", "Cumberland"),
    population = c(1000000, 500000)
  )
  
  data %>%
    group_by(county, town, year, month_num, crime_type) %>%
    summarise(crime_count = n(), .groups = "drop") %>%
    left_join(county_pop, by = "county") %>%
    mutate(crime_rate_per_10k = (crime_count / population) * 10000)
}

# =============================================================================
# 5. MAIN EXECUTION
# =============================================================================

main <- function() {
  cat("=== Starting Data Cleaning ===\n")
  
  hp <- clean_house_price_data()
  bb <- clean_broadband_data()
  cr <- clean_crime_data()
  
  hp_agg <- aggregate_house_prices(hp)
  bb_agg <- aggregate_broadband(bb)
  cr_agg <- aggregate_crime(cr)
  
  write_csv(hp, "cleaned_data/house_price_cleaned.csv")
  write_csv(hp_agg, "cleaned_data/house_price_aggregated.csv")
  write_csv(bb, "cleaned_data/broadband_cleaned.csv")
  write_csv(bb_agg, "cleaned_data/broadband_aggregated.csv")
  write_csv(cr, "cleaned_data/crime_cleaned.csv")
  write_csv(cr_agg, "cleaned_data/crime_aggregated.csv")
  -
  cat("=== Data Cleaning Complete ===\n")
}

# Run
main()
