#' Aggregate data to hourly data.

#' @param dataframe An nx2 time series.
#' @param operation A character string identifying the operation to perform while aggregating the data: Mean, Sum.
#' @return The original dataframe aggregated to hourly data interval.
#'
#' @export

agg_to_hourly <- function(dataframe, operation){

  names(dataframe) <- c("time", "data")

  dts_1 <- lubridate::floor_date(dataframe$time, "hour")
  dataframe$days_hours <- dts_1
  days_hours <- unique(dataframe$days_hours)
  n_days_hours <- length(days_hours)

  ncolumns <- 2

  data_hourly <- as.data.frame(matrix(nrow = n_days_hours, ncol = ncolumns))
  names(data_hourly) <- c("days_hours", "data")

  data_hourly$days_hours <- days_hours

  for (k in 1:n_days_hours){
    k_day_hour <- days_hours[k]
    idx_k_day_hour <- which(dataframe$days_hours == k_day_hour)
    if (operation == "Mean") {
      data_hourly$data[k] <- mean(dataframe$data[idx_k_day_hour], na.rm = T)
    } else if (operation == "Sum") {
      data_hourly$data[k] <- sum(dataframe$data[idx_k_day_hour], na.rm = T)
    }

    names(data_hourly) <- c("time", "data")
  }

  return(data_hourly)
}
