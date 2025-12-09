rda_plot_dir <- "~/Desktop/Datascience in R/final_project/research_driven_analysis/plots"

# highlights early and late routes for plotting
hourly_result <- full_summary %>%
  mutate(
    highlight = case_when(
      Route %in% late_subset$Route ~ "Late",
      Route %in% early_subset$Route ~ "Early",
      TRUE ~ "Other"
    )
  )

total_ridership_plot <- ggplot(hourly_result, aes(x = Hour, y = total_riders, group = Route)) +
  geom_line(aes(color = highlight), size = 1) +
  scale_color_manual(values = c(
    "Late" = "red",
    "Early" = "blue",
    "Other" = "gray70"
  ))  +
  labs(
    title = "Ridership per Hour by Route",
    x = "Hour of Day",
    y = "Total Riders",
    color = "Group"
  ) +
  theme_minimal()
ggsave(paste0(rda_plot_dir,"/total_ridership_plot.png"), total_ridership_plot, width = 8, height = 5)

late_routes_plot <- ggplot(late_subset, aes(x = Hour, y = total_riders, color = as.factor(Route))) +
  geom_line(size = 1) +
  labs(
    title = "Routes that are Delayed by 5 minutes or More",
    x = "Hour of Day",
    y = "Total Riders",
    color = "Route"
  ) +
  theme_minimal()
ggsave(paste0(rda_plot_dir,"/late_routes_plot.png"), late_routes_plot, width = 8, height = 5)

early_routes_plot <- ggplot(early_subset, aes(x = Hour, y = total_riders, color = as.factor(Route))) +
  geom_line(size = 1) +
  labs(
    title = "Routes that are Early by 5 minutes or More",
    x = "Hour of Day",
    y = "Total Riders",
    color = "Route"
  ) +
  theme_minimal()
ggsave(paste0(rda_plot_dir,"/early_routes_plot.png"), early_routes_plot, width = 8, height = 5)

# gets full ridership data for routes that are both early and late
priority_routes <- common_routes(early_subset, late_subset)
priority_routes <- filter_routes(full_summary, priority_routes)