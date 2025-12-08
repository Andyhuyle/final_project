# otp univariate analysis -------------
otp_plots_dir <- "~/Desktop/Datascience in R/final_project/plots/univariate"

# counts for days in may
otp_dates <- ggplot(otp_simulated, aes(x = Date)) +
  geom_histogram(stat = "count", fill = "steelblue", color = "black") +
  labs(title = "Number of Records per Date",
       x = "Date",
       y = "Count") +
  theme_minimal()
ggsave(paste0(otp_plots_dir,"/otp_dates.png"), otp_dates, width = 8, height = 5)

# route
# fct_infreq sorts variable counts
otp_routes <- ggplot(otp_simulated, aes(x = fct_infreq(as.factor(Route)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Count by Route", x = "Route", y = "Count") +
  theme(axis.text.x = element_text(angle=45, size=6, hjust = 1))
ggsave(paste0(otp_plots_dir,"/otp_routes.png"), otp_routes, width = 8, height = 5)

# Driver.ID
otp_drivers <- ggplot(otp_simulated, aes(x = fct_infreq(as.factor(Driver.ID)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Count by Driver.ID", x = "Driver.ID", y = "Count") +
  theme(axis.text.x = element_text(angle=90, size=4, hjust = 1))
ggsave(paste0(otp_plots_dir,"/otp_drivers.png"), otp_drivers, width = 8, height = 5)

# stops
otp_stops <- ggplot(otp_simulated, aes(x = fct_infreq(as.factor(Stop)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Count by Stop Name", x = "Stop", y = "Count") +
  theme(axis.text.x = element_text(angle=70, size=10, hjust = 1))
ggsave(paste0(otp_plots_dir,"/otp_stops.png"), otp_stops, width = 36, height = 12)

# delays
otp_delays <- ggplot(otp_simulated, aes(x = Delay.Sec)) +
  geom_histogram(stat = "count", fill = "steelblue", color = "black") +
  labs(title = "Count by Delays",
       x = "Date",
       y = "Count") +
  theme_minimal()
ggsave(paste0(otp_plots_dir,"/otp_delays.png"), otp_delays, width = 8, height = 5)

# ridership univariate analysis -------------
ridership_plots_dir <- "~/Desktop/Datascience in R/final_project/plots/univariate"

# extracts hour from time col
ridership_simulated <- ridership_simulated %>%
  mutate(
    Time_parsed = mdy_hm(Time),
    Hour = hour(Time_parsed)
  )

# rides per hour of day
ridership_hours <- ggplot(ridership_simulated, aes(x = Hour)) +
  geom_histogram(stat = "count", fill = "steelblue", color = "black") +
  labs(title = "Rides Recorded per Hour of the Day (Simulated Ridership)",
       x = "Hour",
       y = "Count") +
  theme_minimal()
ggsave(paste0(ridership_plots_dir,"/ridership_hours.png"), ridership_hours, width = 8, height = 5)

# rides per day of week
week_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
ridership_days <- ggplot(ridership_simulated, aes(x = factor(Day.of.Week, levels = week_order))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Ridership Days of the Week", x = "Day", y = "Count") +
  theme_minimal()
ggsave(paste0(ridership_plots_dir,"/ridership_days.png"), ridership_days, width = 8, height = 5)

# ridership per route
ridership_routes <- ggplot(ridership_simulated, aes(x = fct_infreq(as.factor(Route)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Ridership by Route", x = "Route", y = "Count") +
  theme(axis.text.x = element_text(angle=45, size=6, hjust = 1))
ggsave(paste0(ridership_plots_dir,"/ridership_routes.png"), ridership_routes, width = 8, height = 5)

# ridership by rider type
ridership_type <- ggplot(ridership_simulated, aes(x = fct_infreq(as.factor(Type)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Ridership by Type of Rider", x = "Type", y = "Count") +
  theme(axis.text.x = element_text(angle=45, size=6, hjust = 1))
ggsave(paste0(ridership_plots_dir,"/ridership_type.png"), ridership_type, width = 8, height = 5)

# ridership by pass
pass_columns <- c("Low.Income", "Transfer", "Off.Peak",
                  "Eco.Pass", "One.Hour.Pass", "Two.Hour.Pass", "Day.Pass",
                  "Ten.Ride.Pass", "Week.Pass", "Monthly.Pass")
# coutns 1s in each column
category_counts <- data.frame(
  Category = pass_columns,
  Count = c(
    sum(ridership$Low.Income == 1, na.rm = TRUE),
    sum(ridership$Transfer == 1, na.rm = TRUE),
    sum(ridership$Off.Peak == 1, na.rm = TRUE),
    sum(ridership$Eco.Pass == 1, na.rm = TRUE),
    sum(ridership$One.Hour.Pass == 1, na.rm = TRUE),
    sum(ridership$Two.Hour.Pass == 1, na.rm = TRUE),
    sum(ridership$Day.Pass == 1, na.rm = TRUE),
    sum(ridership$Ten.Ride.Pass == 1, na.rm = TRUE),
    sum(ridership$Week.Pass == 1, na.rm = TRUE),
    sum(ridership$Monthly.Pass == 1, na.rm = TRUE)
  )
) %>%
  arrange(desc(Count))
ridership_pass <- ggplot(category_counts, aes(x = reorder(Category, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Count by Pass Category",
       x = "Category",
       y = "Count") +
  theme_minimal()
ggsave(paste0(ridership_plots_dir,"/ridership_pass.png"), ridership_pass, width = 8, height = 5)

# ridership by College
as.factor(ridership_simulated$College)
ridership_college <- ggplot(ridership_simulated, aes(x = fct_infreq(as.factor(College)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Ridership by College", x = "College", y = "Count") +
  theme(axis.text.x = element_text(angle=45, size=6, hjust = 1))
ggsave(paste0(ridership_plots_dir,"/ridership_college.png"), ridership_college, width = 8, height = 5)

# ridership by High.School
as.factor(ridership_simulated$High.School)
ridership_highschool <- ggplot(ridership_simulated, aes(x = fct_infreq(as.factor(High.School)))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Ridership by College", x = "College", y = "Count") +
  theme(axis.text.x = element_text(angle=45, size=6, hjust = 1))
ggsave(paste0(ridership_plots_dir,"/ridership_highschool.png"), ridership_highschool, width = 8, height = 5)
