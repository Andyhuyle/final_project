early_and_late_routes_plot <- ggplot(priority_routes, aes(x = Hour, y = total_riders, color = as.factor(Route))) +
  geom_line(size = 1) +
  labs(
    title = "High-Priority Stops: Both Early and Late!",
    x = "Hour of Day",
    y = "Total Riders",
    color = "Route"
  ) +
  theme_minimal()
ggsave(paste0(rda_plot_dir,"/early_and_late_routes_plot.png"), early_and_late_routes_plot, width = 8, height = 5)

priority_routes_heatmap <- ggplot(priority_routes, aes(x = Hour, y = factor(Route), fill = avg_delay_min)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",       # early
    mid = "white",     # on-time
    high = "red",       # late
    midpoint = 0,
    limits = c(-20, 20),
    name = "Avg Delay (min)"
  ) +
  labs(
    x = "Hour of Day",
    y = "Route",
    title = "Heatmap of Average Delay by Route and Hour"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(paste0(rda_plot_dir,"/priority_routes_heatmap.png"), priority_routes_heatmap, width = 8, height = 5)

priority_ridership_heatmap <- ggplot(priority_routes, aes(x = Hour, y = factor(Route), fill = avg_delay_min, alpha = total_riders)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-5,5), name = "Avg Delay (min)") +
  scale_alpha(range = c(0.3, 1), name = "Riders (opacity)") +
  labs(x = "Hour of Day", y = "Route", title = "Delay by Hour and Route (Alpha = Riders)") +
  theme_minimal()
ggsave(paste0(rda_plot_dir,"/priority_ridership_heatmap.png"), priority_ridership_heatmap, width = 8, height = 5)