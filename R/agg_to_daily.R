#' Aggregate data to daily data

#' @param eload_data A dataframe with energy consumption/demand time series.
#'   Column names: "time" and "eload".
#' @param temp_data A dataframe with weather time series.
#'   Column names: "time" and "temp".
#' @return The orginal dataframe aggregated to daily data interval
#'
#' @export

agg_to_daily <- function(eload_data = NULL, temp_data = NULL, temp_balancepoint){

  # temp data
  dts_2 <- lubridate::floor_date(temp_data$time, "day")
  temp_data$days <- dts_2
  days <- unique(temp_data$days)
  n_days <- length(days)

  ncolumns <- 2

  temp_daily <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))
  names(temp_daily) <- c("days", "temp")
  temp_daily$days <- days
  for (k in 1:n_days){
    k_day <- days[k]
    idx_k_day <- which(temp_data$days == k_day)
    temp_daily$temp[k] <- mean(temp_data$temp[idx_k_day], na.rm = T)
    names(temp_daily) <- c("time", "temp")
  }

  # eload data
  dts_2 <- lubridate::floor_date(eload_data$time, "day")
  eload_data$days <- dts_2
  days <- unique(eload_data$days)
  n_days <- length(days)
  ncolumns <- 2
  eload_daily <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))
  names(eload_daily) <- c("days", "eload")
  eload_daily$days <- days
  for (k in 1:n_days){
    k_day <- days[k]
    idx_k_day <- which(eload_data$days == k_day)
    eload_daily$eload[k] <- sum(eload_data$eload[idx_k_day],
                                  na.rm = T)
    names(eload_daily) <- c("time", "eload")
  }

  # join eload and temp data

  data_daily <- dplyr::inner_join(eload_daily, temp_daily, by = "time")

  # compute HDD and CDD
  temp_daily_col <- data_daily$temp
  base_temp <- rep(temp_balancepoint, length(temp_daily_col))
  HDD <- base_temp - temp_daily_col
  HDD[HDD < 0 ] <- 0
  CDD <- temp_daily_col - base_temp
  CDD[CDD < 0 ] <- 0
  data_daily <-  cbind(data_daily, HDD, CDD)

  data_daily <- data_daily[complete.cases(data_daily), ]
  names(data_daily) <- c("time", "eload", "temp", "HDD", "CDD")

  return(data_daily)
}
