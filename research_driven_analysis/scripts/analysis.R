#' Generate Hourly Summary with Priority Categories
#'
#' This function computes a priority level based on ridership and average delay,
#' and outputs multiple data frames for each priority category.
#'
#' @param ridership_agg A dataframe containing aggregated ridership per Route and Hour.
#'   Need columns: Route, Hour, total_riders.
#' @param schedule_agg A data rame containing aggregated schedule per Route and Hour.
#'   Need columns: Route, Hour, avg_delay_min.
#'
#' @return A named list of data frames:
#'   \item{summary}{Full hourly summary with priority_expand column}
#'   \item{Early}{Rows with priority_expand == "Early"}
#'   \item{Late}{Rows with priority_expand == "Late"}
#'   \item{On_Time}{Rows with priority_expand == "On_Time"}
#'
#' @examples
#' result <- generate_hourly_summary(ridership_aggregated, otp_aggregated)
#' result$high_priority
#'
#' @export
assign_and_split_priority <- function(merged_dataset) {
  hourly_summary <- merged_dataset %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
    arrange(Route, Hour) %>%
    mutate(priority_expand = case_when(
      avg_delay_min < -5 ~ "Early",
      avg_delay_min > 5 ~ "Late",
      TRUE ~ "On_Time"
    ))
  
  # split by priority
  Early <- hourly_summary %>% filter(priority_expand == "Early")
  Late <- hourly_summary %>% filter(priority_expand == "Late")
  On_Time <- hourly_summary %>% filter(priority_expand == "On_Time")
  
  list(
    summary = hourly_summary,
    Early = Early,
    Late = Late,
    On_Time = On_Time
  )
}
