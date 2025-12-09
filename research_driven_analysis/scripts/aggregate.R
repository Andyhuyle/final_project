#' aggregate_ridership
#'
#' Aggregates and summarizes simulated ridership data
#'
#' @param ridership_data dataframe of simulated ridership data
#' @return Dataframe of aggregated ridership data sorted by Route and Stop.Number
#' 
#' @example 
#' ridership_aggregated <- aggregate_ridership(ridership_simulated)
#'
#' @export 
aggregate_ridership <- function(ridership_data) {
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
  
  return(ridership_summary)
}

#' aggregate_otp
#'
#' Aggregates and summarizes otp data
#'
#' @param otp_data dataframe of simulated otp data
#' @param delay_threshold time threshold to consider a bus stop late
#' 
#' @return Dataframe of aggregated otp data sorted by Route and Stop
#' 
#' @example 
#' ridershp_aggregated <- aggregate_ridership(ridership_simulated)
#' 
#' @export 
aggregate_otp <- function(otp_data) {
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
  
  return(otp_summary)
}
