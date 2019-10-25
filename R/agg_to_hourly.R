#' Aggregate data to hourly data.

#' @param eload_data A dataframe with energy consumption/demand time series.
#'   Column names: "time" and "eload".
#' @param temp_data A dataframe with weather time series.
#'   Column names: "time" and "temp".
#' @return The original dataframe aggregated to hourly data interval.
#'
#' @export

agg_to_hourly <- function(eload_data = NULL, temp_data = NULL){

  # temp data

  dts_1 <- lubridate::floor_date(temp_data$time, "hour")
  temp_data$days_hours <- dts_1
  days_hours <- unique(temp_data$days_hours)
  n_days_hours <- length(days_hours)

  ncolumns <- 2

  temp_hourly <- as.data.frame(matrix(nrow = n_days_hours, ncol = ncolumns))
  names(temp_hourly) <- c("days_hours", "temp")
  temp_hourly$days_hours <- days_hours

  for (k in 1:n_days_hours){
    k_day_hour <- days_hours[k]
    idx_k_day_hour <- which(temp_data$days_hours == k_day_hour)
    temp_hourly$temp[k] <- mean(temp_data$temp[idx_k_day_hour], na.rm = T)
    names(temp_hourly) <- c("time", "temp")
  }

  # eload data
  dts_1 <- lubridate::floor_date(eload_data$time, "hour")
  eload_data$days_hours <- dts_1
  days_hours <- unique(eload_data$days_hours)
  n_days_hours <- length(days_hours)

  ncolumns <- 2

  eload_hourly <- as.data.frame(matrix(nrow = n_days_hours, ncol = ncolumns))
  names(eload_hourly) <- c("days_hours", "eload")
  eload_hourly$days_hours <- days_hours

  for (k in 1:n_days_hours){
    k_day_hour <- days_hours[k]
    idx_k_day_hour <- which(eload_data$days_hours == k_day_hour)
    eload_hourly$eload[k] <- sum(eload_data$eload[idx_k_day_hour])
    names(eload_hourly) <- c("time", "eload")
  }

  # join eload and temp data

  data_hourly <- dplyr::inner_join(eload_hourly, temp_hourly, by = "time")
  data_hourly <- data_hourly[complete.cases(data_hourly), ]
  names(data_hourly) <- c("time", "eload", "temp")

  return(data_hourly)
}
