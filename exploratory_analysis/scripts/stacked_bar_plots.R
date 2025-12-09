# stacked bar plots
library(dplyr)
library(ggplot2)
library(tidyr)

# Part 1: By Stop + Demographics 
# count the number of riders at each stop, grouped by type (child, adult, disabled, senior, employee/spouse/retiree, transfer)
rider_count <- ridership_simulated %>%
  group_by(Stop.Number, Type) %>%
  summarise(n = n(), .groups = "drop")

# calculate proportions of total riders at each stop
rider_proportions <- rider_count %>%
  group_by(Stop.Number) %>%
  mutate(prop = n / sum(n))

# generate corresponding bar graph
ggplot(rider_proportions, aes(x = factor(Stop.Number), y = prop, fill = Type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportional Rider Demographics by Stop",
    x = "Stop Number",
    y = "Proportion of Riders",
    fill = "Rider Type"
  ) +
  theme_minimal(base_size = 14, ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 6
    )
  )

# Part 2: By Route + Demographics 
# count the number of riders on each route, grouped by type (child, adult, disabled, senior, employee/spouse/retiree, transfer)
counts <- ridership_simulated %>%
  group_by(Route, Type) %>%
  summarise(n = n(), .groups = "drop")

# calculate proportions of total riders at each stop
props <- counts %>%
  group_by(Route) %>%
  mutate(prop = n / sum(n))

# generate corresponding bar graph
ggplot(props, aes(x = factor(Route), y = prop, fill = Type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportional Rider Demographics by Route",
    x = "Route",
    y = "Proportion of Riders",
    fill = "Rider Type"
  ) +
  theme_minimal(base_size = 14, ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 6
    )
  )

# Part 3: By Income + Demographics 
# count the number of riders who are low income vs not low income, grouped by type (child, adult, disabled, senior, employee/spouse/retiree, transfer)
rider_count <- ridership_simulated %>%
  group_by(Low.Income, Type) %>%
  summarise(n = n(), .groups = "drop")

# calculate proportions of total riders who are low income vs not low income
rider_proportions <- rider_count %>%
  group_by(Low.Income) %>%
  mutate(prop = n / sum(n))

# generate corresponding bar graph
ggplot(rider_proportions, aes(x = factor(Low.Income), y = prop, fill = Type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportional Rider Demographics by Income",
    x = "Low.Income",
    y = "Proportion of Riders",
    fill = "Rider Type"
  ) +
  theme_minimal(base_size = 14, )

# Part 4: By College
# count the number of riders at each college
college_count <- ridership_simulated %>%
  group_by(College) %>%
  summarise(n = n(), .groups = "drop")

# calculate proportions of total riders at each college
college_proportions <- college_count %>%
  filter(College != "None") %>%
  mutate(prop = n / sum(n))

# generate corresponding bar graph
ggplot(college_proportions, aes(x = 1, y = prop, fill = College)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Proportion of Riders by College",
    x = "",
    y = "Proportion of Riders",
    fill = "College"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Part 5: By High School 
# count the number of riders at each high school
hs_count <- ridership_simulated %>%
  group_by(High.School) %>%
  summarise(n = n(), .groups = "drop")

# calculate proportions of total riders at each high school
hs_proportions <- hs_count %>%
  filter(High.School != "None") %>%
  mutate(prop = n / sum(n))

# generate corresponding bar graph
ggplot(hs_proportions, aes(x = "", y = prop, fill = High.School)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # convert to pie chart
  labs(
    title = "Proportion of Riders by High School",
    fill = "High School"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
