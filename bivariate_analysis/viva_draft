library(tidyverse)
library(tidyr)
library(dplyr) # to use %>%
library(readr) # to read excel file
library(lubridate) # to use as.Date function
otp_simulated <- read_csv("C://Users/vivav/Downloads/otp_simulated.csv")
ridership_simulated <- read_csv("C://Users/vivav/Downloads/ridership_simulated.csv")

names(otp_simulated)
names(ridership_simulated)

otp_simulated <- otp_simulated %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Scheduled.Time = mdy_hm(Scheduled.Time),
    Actual.Arrival.Time = hms(Actual.Arrival.Time),
    Weekdays = weekdays(as.Date(Date, format = "%m/%d/%Y"))
  )

ridership_simulated <- ridership_simulated %>%
  mutate(Scheduled.Time = mdy_hm(Time))

unique(otp_simulated$Route)
unique(ridership_simulated$Route)

unique(otp_simulated$Trip)
unique(ridership_simulated$Trip)
# trips are not consistent across dataframes

combined <- otp_simulated %>%
  inner_join(
    ridership_simulated,
    by = c("Scheduled.Time", "Route"))

time_summary <- combined %>%
  mutate(
    Day = wday(Date, label = TRUE, week_start = 1),
    hour = hour(Actual.Arrival.Time)
  ) %>%
  group_by(Day, hour) %>%
  summarise(
    total_trips = n(),
    mean_delay_sec = mean(Delay.Sec, na.rm = TRUE),
    num_delayed = sum(Delay.Sec > 0, na.rm = TRUE),
    pct_delayed = mean(Delay.Sec > 0, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(time_summary, aes(x = hour, y = pct_delayed, color = Day)) +
  geom_line() +
  labs(y = "Percent delayed", x = "Hour of day")

daily_delays <- combined %>%
  group_by(Date) %>%
  summarise(
    total_trips = n(),
    num_delayed = sum(Delay.Sec > 0, na.rm = TRUE),
    pct_delayed = mean(Delay.Sec > 0, na.rm = TRUE),
    mean_delay = mean(Delay.Sec, na.rm = TRUE),
    .groups = "drop"
  )

weekly_delays <- combined %>%
  mutate(week = week(Date)) %>%
  group_by(week) %>%
  summarise(
    total_trips = n(),
    num_delayed = sum(Delay.Sec > 0, na.rm = TRUE),
    pct_delayed = mean(Delay.Sec > 0, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(daily_delays, aes(x = Date, y = pct_delayed)) +
  geom_line() +
  labs(y = "Percent delayed", x = "Day")

ggplot(daily_delays, aes(x = total_trips, y = pct_delayed)) +
  geom_line() +
  labs(y = "Percent delayed", x = "Total Trips")

stop_summary <- combined %>%
  group_by(Stop, Date) %>%
  summarise(
    boardings = sum(Ride.Count, na.rm = TRUE),
    mean_delay = mean(Delay.Sec, na.rm = TRUE),
    num_delayed = sum(Delay.Sec > 0, na.rm = TRUE),
    pct_delayed = mean(Delay.Sec > 0, na.rm = TRUE),
    .groups = "drop"
  )

demographics_by_stop <- ridership_simulated %>%
  group_by(Stop.Number) %>%
  summarise(
    total_riders = sum(Ride.Count, na.rm = TRUE),
    pct_college = mean(College == 1, na.rm = TRUE),
    pct_highschool = mean(High.School == 1, na.rm = TRUE),
    pct_low_income = mean(Low.Income == 1, na.rm = TRUE),
    .groups = "drop"
  )
