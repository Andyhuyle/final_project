#' aggregate_ridership
#'
#' Aggregates and summarizes simulated ridership data
#'
#' @param ridership_data dataframe of simulated ridership data
#' @return Dataframe of aggregated ridership data sorted by Route and Stop.Number
#' @export datagrame of aggregated ridership data with columns: Route, Stop.Number,
#'total_riders, num_boardings, low_income_riders, student_riders, and 
#'vulnerable_prop
aggregate_ridership <- function(ridership_data) {
  cat("\nAggregating ridership data...\n")
  
  ridership_summary <- ridership_data %>%
    mutate(Time = trimws(Time),            # remove stray spaces
           Time = mdy_hm(Time),
           Hour = hour(Time)) %>%
    group_by(Route, Hour) %>%
    summarize(
      total_riders = sum(Ride.Count, na.rm = TRUE),
      num_boardings = n(),
      low_income_riders = sum(Low.Income, na.rm = TRUE),
      student_riders = sum(College != "None") + sum(High.School != "None"),
      vulnerable_prop = (low_income_riders + student_riders) / total_riders,
      .groups = "drop"
    ) %>%
    arrange(Route)
  
  cat("Aggregated to", nrow(ridership_summary), "unique Route-Stop combinations\n")
  return(ridership_summary)
}

#' aggregate_otp
#'
#' Aggregates and summarizes otp data
#'
#' @param otp_data dataframe of simulated otp data
#' @param delay_threshold time threshold to consider a bus stop late
#' @return Dataframe of aggregated otp data sorted by Route and Stop
#' @export datagrame of aggregated otp data with columns: Route, Stop,
#'total_trips, delayed_trips, delay_rate, avg_delay_sec, and 
#'avg_delay_min
aggregate_otp <- function(otp_data) {
  cat("\nAggregating OTP data...\n")
  
  otp_summary <- otp_data %>%
    mutate(Hour = hour(Actual.Arrival.Time)) %>%
    group_by(Route, Hour) %>%
    summarize(
      total_trips = n(),
      avg_delay_sec = round(mean(Delay.Sec, na.rm = TRUE), 3),
      avg_delay_min = round(avg_delay_sec / 60, 3),
      .groups = "drop"
    ) %>%
    arrange(Route)
  
  cat("Aggregated to", nrow(otp_summary), "unique Route-Stop combinations\n")
  return(otp_summary)
}
