#' Merge ridership and otp Data into Hourly Summary
#'
#' This function takes aggregated ridership and otp datasets, merges them by Route and Hour,
#' replaces missing values with 0, and returns a cleaned hourly summary.
#'
#' @param ridership_agg A data frame containing aggregated ridership per Route and Hour.
#'   Must include columns: Route, Hour, total_riders.
#' @param schedule_agg A data frame containing aggregated schedule data per Route and Hour.
#'   Must include columns: Route, Hour, avg_delay_min.
#'
#' @return A data frame containing the merged hourly summary with NAs replaced by 0,
#'   sorted by Route and Hour.
#'
#' @examples
#' hourly_summary <- make_hourly_summary(ridership_aggregated, otp_aggregated)
#'
#' @export
map_ridership <- function(ridership_agg, otp_agg) {
  hourly_summary <- ridership_agg %>%
    full_join(otp_agg, by = c("Route", "Hour")) %>%
    # replace NAs with 0 for key columns
    mutate(
      total_riders = ifelse(is.na(total_riders), 0, total_riders),
      avg_delay_min = ifelse(is.na(avg_delay_min), 0, avg_delay_min)
    ) %>%
    arrange(Route, Hour)
  
  # replace any remaining NAs with 0
  hourly_summary[is.na(hourly_summary)] <- 0
  
  return(hourly_summary)
}
