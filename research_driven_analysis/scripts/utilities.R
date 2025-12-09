#' Filter Hourly Summary by Multiple Routes
#'
#' This function filters a data frame (e.g., hourly summary or priority subset)
#' for one or more routes.
#'
#' @param df A data frame containing at least the column `Route`.
#' @param route_ids A vector of route numbers or names to filter for.
#'
#' @return A data frame containing only rows for the specified routes.
#'
#' @examples
#' selected_routes <- c(13, 28, 66)
#' route_data <- filter_routes(priority_result$summary, selected_routes)
#'
#' @export
filter_routes <- function(df, route_ids) {
  df %>%
    dplyr::filter(Route %in% route_ids)
}

#' Get Routes Common to Two Datasets
#'
#' This function identifies routes that are present in both data frames and
#' optionally returns filtered versions of the datasets containing only those routes.
#'
#' @param df1 First data frame containing a column `Route`.
#' @param df2 Second data frame containing a column `Route`.
#' @param filter_data Logical, if TRUE the function also returns filtered data frames.
#'
#' @return If filter_data = FALSE: a vector of common Route IDs.
#'         If filter_data = TRUE: a list containing
#'           \item{common_routes}{vector of common Route IDs}
#'           \item{df1_filtered}{df1 with only common routes}
#'           \item{df2_filtered}{df2 with only common routes}
#'
#' @examples
#' result <- common_routes(df1, df2, filter_data = TRUE)
#' result$common_routes
#' result$df1_filtered
#' result$df2_filtered
#'
#' @export
common_routes <- function(df1, df2) {
  # Get unique Route values
  routes_df1 <- unique(df1$Route)
  routes_df2 <- unique(df2$Route)
  
  # Find intersection
  common <- intersect(routes_df1, routes_df2)
}

