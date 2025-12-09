#' Filter a df by Multiple Routes
#'
#' This function filters a data frame (ex hourly summary or priority subset)
#' for one or more routes.
#'
#' @param df A data frame 
#' @param route_ids A vector of route numbers or names to filter for.
#'
#' @return A data frame containing only rows for the specified routes.
#'
#' @examples
#' selected_routes <- c(13, 28, 66)
#' route_data <- filter_routes(hourly_summary$summary, selected_routes)
#'
#' @export
filter_routes <- function(df, route_ids) {
  df %>%
    dplyr::filter(Route %in% route_ids)
}


#' Get Routes Common to Two Datasets
#'
#' This function identifies routes that are present in both data frames
#'
#' @param df1 First data frame containing a route column.
#' @param df2 Second data frame containing a route column`.
#'
#' @return A vector of routes (numerical)
#'
#' @examples
#' result <- common_routes(df1, df2)
#' -> c(13, 28, 66)
#'
#' @export
common_routes <- function(df1, df2) {
  routes_df1 <- unique(df1$Route)
  routes_df2 <- unique(df2$Route)
  
  common <- intersect(routes_df1, routes_df2)
  return(common)
}

