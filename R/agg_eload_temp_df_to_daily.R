#' Aggregate 15 minute or hourly dataframe to a daily interval.

#' @param eload_data A dataframe with energy consumption/demand time series.
#'   Column names: "time" and "eload".
#' @param temp_data A dataframe with weather time series.
#'   Column names: "time" and "temp".
#' @param data_is_quantity A Boolean. TRUE indicates interval data as quantity while
#'   FALSE indicates interval data as rate.
#' @param balancepoint_temp A numeric equal to the balancepoint temperature for
#'   calculating HDD and CDD.
#' @return The orginal dataframe aggregated to daily data interval.
#'
#' @export


agg_eload_temp_df_to_daily <- function(eload_data, temp_data,
                                                 data_is_quantity = T,
                                                 balancepoint_temp){

  if (is.character(eload_data$time)) {
    eload_data$time <- lubridate::mdy_hm(eload_data$time)
  }

  if (is.character(eload_data$time)) {
    temp_data$time <- lubridate::mdy_hm(temp_data$time)
  }

  eload_data <- eload_data[complete.cases(eload_data), ]
  temp_data <- temp_data[complete.cases(temp_data), ]


  # Check interval of temp data
  nterval <- difftime(temp_data$time[2], temp_data$time[1], units = "min")

  if (nterval == 60) {
    data_interval <- "Hourly"
  } else if (nterval == 15) {
    data_interval <- "15-min"
  } else if (nterval == 1440) {
    data_interval <- "Daily"
  }

  if (data_interval == "15-min" | data_interval == "Hourly") {
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
  } else if (data_interval == "Daily") {
    temp_daily <- temp_data[, c("time", "temp")]
  }

  # Check interval of eload data
  nterval_eload <- difftime(eload_data$time[2], eload_data$time[1],
                            units = "min")

  if (nterval_eload == 60) {
    data_interval <- "Hourly"
  } else if (nterval_eload == 15) {
    data_interval <- "15-min"
  } else if (nterval_eload == 1440) {
    data_interval <- "Daily"
  }

  if (data_interval == "15-min" | data_interval == "Hourly") {
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
      if (data_is_quantity){
        eload_daily$eload[k] <- sum(eload_data$eload[idx_k_day],
                                    na.rm = T)
      } else {
        eload_daily$eload[k] <- mean(eload_data$eload[idx_k_day],
                                     na.rm = T) * 24
      }
    }

    names(eload_daily) <- c("time", "eload")

  } else if (data_interval == "Daily") {
    eload_daily <- eload_data[, c("time", "eload")]
  }

  data_daily <- dplyr::inner_join(eload_daily, temp_daily, by = "time")
  temp_daily_col <- data_daily$temp
  base_temp <- rep(balancepoint_temp, length(temp_daily_col))
  HDD <- base_temp - temp_daily_col
  HDD[HDD < 0 ] <- 0
  CDD <- temp_daily_col - base_temp
  CDD[CDD < 0 ] <- 0
  data_daily <-  cbind(data_daily, HDD, CDD)
  data_daily <- data_daily[complete.cases(data_daily), ]
  names(data_daily) <- c("time", "eload", "temp", "HDD", "CDD")
  return(data_daily)

}
