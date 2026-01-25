# =============================================================================
# 1a) BOXPLOT: House Price Distribution (2024) by Town and County
# =============================================================================

hp_2024 <- house_price_data %>%
  filter(year == 2024)

plot_1a <- ggplot(hp_2024, aes(x = town, y = price, fill = town)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1) +
  facet_wrap(~ county, scales = "free_x") +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 1000000)) +
  labs(
    title = "House Price Distribution by Town (2024)",
    subtitle = "Cheshire vs Cumberland",
    x = "Town",
    y = "House Price (£)",
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
  filename = file.path(base_path, "Charts/house_price_boxplot_2024_by_town.png"),
  plot = plot_1a,
  width = 11,
  height = 6,
  dpi = 300
)

cat("Saved: house_price_boxplot_2024_by_town.png\n")
