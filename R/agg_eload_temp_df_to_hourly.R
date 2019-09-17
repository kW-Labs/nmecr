#' Aggregate eload and temp dataframe to hourly interval data

#' @param eload_data A tidy dataframe with energy consumption/demand time series.
#'   Column names: "time" and "eload".
#' @param temp_data A tidy dataframe with weather time series.
#'   Column names: "time" and "temp".
#' @param data_is_quantity A Boolean. TRUE indicates interval data as quantity while
#'   FALSE indicates interval data as rate.
#' @return The orginal dataframe aggregated to hourly data interval.
#'
#' @export


agg_eload_temp_df_to_hourly <- function(eload_data, temp_data,
                                        data_is_quantity = T){

  if (is.character(eload_data$time)) {
    eload_data$time <- lubridate::mdy_hm(eload_data$time)
  }

  if (is.character(temp_data$time)) {
    temp_data$time <- lubridate::mdy_hm(temp_data$time)
  }

  eload_data <- eload_data[complete.cases(eload_data), ]
  temp_data <- temp_data[complete.cases(temp_data), ]

  # Temperature Data
  nterval_temp <- difftime(temp_data$time[2], temp_data$time[1], units = "min")

  if (nterval_temp == 15) {
    data_interval_temp <- "15-min"
  } else if (nterval_temp < 60) {
    data_interval_temp <- "less than 60-min"
  } else if (nterval_temp == 60) {
    data_interval_temp <- "Hourly"
  } else if (nterval_temp == 1440) {
    data_interval_temp <- "Daily"
  }

  if (data_interval_temp == "15-min" | data_interval_temp == "less than 60-min") {

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

  } else if (data_interval_temp == "Hourly" || data_interval_temp == "Daily") {
    temp_hourly <- temp_data[, c("time", "temp")]
  }

  # Eload Data
  nterval_eload <- difftime(eload_data$time[2], eload_data$time[1], units = "min")

  if (nterval_eload == 15) {
    data_interval_eload <- "15-min"
  } else if (nterval_eload < 60) {
    data_interval_eload <- "less than 60-min"
  } else if (nterval_eload == 60) {
    data_interval_eload <- "Hourly"
  } else if (nterval_eload == 1440) {
    data_interval_eload <- "Daily"
  }

  if (data_interval_eload == "15-min" | data_interval_eload == "less than 60-min") {
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
      if (data_is_quantity){
        eload_hourly$eload[k] <- sum(eload_data$eload[idx_k_day_hour])
      } else{
        eload_hourly$eload[k] <- mean(eload_data$eload[idx_k_day_hour])
      }
      names(eload_hourly) <- c("time", "eload")
    }

  } else if (data_interval_eload == "Hourly" || data_interval_eload == "Daily") {
    eload_hourly <- eload_data[, c("time", "eload")]
  }

  data_hourly <- dplyr::inner_join(eload_hourly, temp_hourly, by = "time")
  data_hourly <- data_hourly[complete.cases(data_hourly), ]
  names(data_hourly) <- c("time", "eload", "temp")
  return(data_hourly)

}
