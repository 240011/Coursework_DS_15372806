# ================================
# BOXPLOT: Drug Offences by Town and County
# June 2024
# ================================

library(tidyverse)
library(scales)

drug_lsoa_town <- crime_data %>%
  filter(
    crime_type == "Drugs",
    year == 2024,
    month_num == 6
  ) %>%
  group_by(county, town, lsoa_name) %>%   # multiple areas per town
  summarise(
    area_count = n(),
    .groups = "drop"
  )

drug_boxplot <- ggplot(drug_lsoa_town, aes(x = town, y = area_count, fill = town)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1) +
  facet_wrap(~ county, scales = "free_x") +
  labs(
    title = "Drug Offence Distribution by Town (June 2024)",
    subtitle = "Cheshire vs Cumberland",
    x = "Town",
    y = "Drug Offences per Area",
    fill = "Town"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(
  filename = "Charts/crime_drug_boxplot_june_2024_by_town.png",
  plot = drug_boxplot,
  width = 11,
  height = 6,
  dpi = 300
)

cat("Saved: crime_drug_boxplot_june_2024_by_town.png\n")
