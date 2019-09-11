#' Aggregate 15 minute, hourly or daily dataframe to a monthly interval.

#' @param eload_data A dataframe with energy consumption/demand time series.
#'   Column names: "time" and "eload". If time series data interval is monthly,
#'   add another column, 'days', listing the number of days in the corresponding
#'   month.
#' @param temp_data A dataframe with weather time series.
#'   Column names: "time" and "temp".
#' @param data_is_quantity A Boolean. TRUE indicates interval data as quantity while
#'   FALSE indicates interval data as rate.
#' @param balancepoint_temp A numeric equal to the balancepoint temperature for
#'   calculating HDD and CDD.
#' @return The orginal dataframe aggregated to monthly data interval.
#'
#' @export

agg_eload_temp_df_to_monthly <- function(eload_data, temp_data,
                               data_is_quantity = T,
                               balancepoint_temp){

  if (is.character(eload_data$time)) {
    eload_data$time <- lubridate::mdy_hm(eload_data$time)
  }

  if (is.character(temp_data$time)) {
    temp_data$time <- lubridate::mdy_hm(temp_data$time)
  }

  eload_data <- eload_data[complete.cases(eload_data), ]
  temp_data <- temp_data[complete.cases(temp_data), ]

  # Check interval of temp data
  nterval <- difftime(temp_data$time[2], temp_data$time[1], units = "min")

  if (nterval == 60) {
    data_interval <- "Hourly"
  } else if (nterval < 60) {
    data_interval <- "15-min"
  } else if (nterval == 1440) {
    data_interval <- "Daily"
  }

  if (data_interval == "Hourly" | data_interval == "15-min") {

    dts_2 <- lubridate::floor_date(temp_data$time, "day")
    temp_data$days <- dts_2
    days <- unique(temp_data$days)
    n_days <- length(days)

    ncolumns <- 2

    data_daily <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))

    names(data_daily) <- c("days", "temp")

    data_daily$days <- days

    for (k in 1:n_days){
      k_day <- days[k]
      idx_k_day <- which(temp_data$days == k_day)
      data_daily$temp[k] <- mean(temp_data$temp[idx_k_day], na.rm = T)

    }

  } else if (data_interval == "Daily") {
    data_daily <- temp_data
    names(data_daily) <- c("days", "temp")
    }

  temp_daily <- data_daily$temp

  base_temp <- rep(balancepoint_temp, length(temp_daily))

  HDD <- base_temp - temp_daily
  HDD[HDD < 0 ] <- 0

  CDD <- temp_daily - base_temp
  CDD[CDD < 0 ] <- 0

  data_daily <-  cbind(data_daily, HDD, CDD)

  monthly_data <- data_daily %>%
    mutate(time = lubridate::floor_date(days, "month"))

  days <- monthly_data %>%
    count(time) %>%
    rename(cdays = n)

  monthly_data <- monthly_data %>%
    group_by(time) %>%
    summarize(temp = mean(temp),
              HDD = sum(HDD),
              CDD = sum(CDD)) %>%
    mutate(days = days[[2]]) %>%
    mutate(HDDperday = HDD / days) %>%
    mutate(CDDperday = CDD / days)


  # Check interval of eload data
    nterval_eload <- difftime(eload_data$time[2], eload_data$time[1],
                              units = "min")

    if (nterval_eload == 15) {
      data_interval_eload <- "15-min"
    } else if (nterval_eload == 60) {
      data_interval_eload <- "Hourly"
    } else if (nterval_eload < 60) {
      data_interval_eload <- "Less than 60"
    } else if (nterval_eload == 1440) {
      data_interval_eload <- "Daily"
    } else if (nterval_eload > 2880) {
      data_interval_eload <- "Monthly"
    }


    if (data_interval_eload == "15-min" | data_interval_eload == "Hourly" |
       data_interval_eload == "Daily" | data_interval_eload == "Less than 60") {

      eload_dts_2 <- lubridate::floor_date(eload_data$time, "day")
      eload_data$days <- eload_dts_2
      days <- unique(eload_data$days)
      n_days <- length(days)

      ncolumns <- 2

      eload_data_daily <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))

      names(eload_data_daily) <- c("time", "eload")

      eload_data_daily$time <- days

      for (k in 1:n_days){
        k_day <- days[k]
        idx_k_day <- which(eload_data$days == k_day)
        if (data_is_quantity){
          eload_data_daily$eload[k] <- sum(eload_data$eload[idx_k_day],
                                           na.rm = T)

        } else {
          eload_data_daily$eload[k] <- mean(eload_data$eload[idx_k_day],
                                                  na.rm = T) * 24

        names(eload_data_daily) <- c("time", "eload")
        }

      }

    } else if (data_interval_eload == "Monthly") {

      eload_data_daily <- eload_data

    }


    if (data_interval_eload == "15-min" | data_interval_eload == "Hourly" |
       data_interval_eload == "Daily" | data_interval_eload == "Less than 60") {

      eload_monthly_data <- eload_data_daily %>%
        mutate(time = lubridate::floor_date(days, "month"))

     days <- eload_monthly_data %>%
      count(time) %>%
      rename(cdays = n)

     eload_monthly_data <- eload_monthly_data %>%
      group_by(time) %>%
      summarize(eload = sum(eload)) %>%
       mutate(days = days[[2]]) %>%
       mutate(eloadperday = eload / days)

    } else {
      eload_monthly_data <- eload_data_daily %>%
          inner_join(days, by = "time") %>%
          mutate(eloadperday = eload / cdays) %>%
          mutate(days = cdays)
      }


  baseline_data <- dplyr::inner_join(monthly_data, eload_monthly_data,
                              by = c("time", "days"))
  baseline_data <- baseline_data[complete.cases(baseline_data), ]

  return(baseline_data)
}
