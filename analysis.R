#' Generate Hourly Summary with Priority Categories
#'
#' This function computes a priority level based on ridership and average delay,
#' and outputs multiple data frames for each priority category.
#'
#' @param ridership_agg A data frame containing aggregated ridership per Route and Hour.
#'   Must include columns: Route, Hour, total_riders.
#' @param schedule_agg A data frame containing aggregated schedule per Route and Hour.
#'   Must include columns: Route, Hour, avg_delay_min.
#'
#' @return A named list of data frames:
#'   \item{summary}{Full hourly summary with priority_expand column}
#'   \item{high_priority}{Rows with priority_expand == "High Priority"}
#'   \item{medium_priority}{Rows with priority_expand == "Medium Priority"}
#'   \item{monitor_delay}{Rows with priority_expand == "Monitor Delay"}
#'   \item{low_priority}{Rows with priority_expand == "Low Priority"}
#'
#' @examples
#' result <- generate_hourly_summary(ridership_aggregated, otp_aggregated)
#' result$high_priority
#'
#' @export
assign_and_split_priority <- function(merged_dataset) {
  # Merge datasets
  hourly_summary <- merged_dataset %>%
    # Replace NAs in key numeric columns
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
    arrange(Route, Hour) %>%
    # Compute priority category
    mutate(priority_expand = case_when(
      avg_delay_min < -5 ~ "Early",
      avg_delay_min > 5 ~ "Late",
      TRUE ~ "On_Time"
    ))
  
  # Split by priority
  Early <- hourly_summary %>% filter(priority_expand == "Early")
  Late <- hourly_summary %>% filter(priority_expand == "Late")
  On_Time <- hourly_summary %>% filter(priority_expand == "On_Time")
  
  # Return as a named list
  list(
    summary = hourly_summary,
    Early = Early,
    Late = Late,
    On_Time = On_Time
  )
}
