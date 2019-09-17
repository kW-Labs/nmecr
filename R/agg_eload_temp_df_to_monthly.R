#' Aggregate eload and temp dataframe to a monthly interval.

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
  } else if (nterval_temp > 2880) {
    data_interval_temp <- "Monthly"
  }

  if (data_interval_temp == "15-min" | data_interval_temp == "less than 60" |
      data_interval_temp == "Hourly" | data_interval_temp == "Daily") {

    dts_2 <- lubridate::floor_date(temp_data$time, "day")
    temp_data$days <- dts_2
    days <- unique(temp_data$days)
    n_days <- length(days)

    ncolumns <- 2

    temp_daily_prelim <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))

    names(temp_daily_prelim) <- c("days", "temp")

    temp_daily_prelim$days <- days

    for (k in 1:n_days){
      k_day <- days[k]
      idx_k_day <- which(temp_data$days == k_day)
      temp_daily_prelim$temp[k] <- mean(temp_data$temp[idx_k_day], na.rm = T)

    }

  } else if (data_interval_temp == "Daily") {
    temp_daily_prelim <- temp_data
    names(temp_daily_prelim) <- c("days", "temp")
    }

  temp_daily <- temp_daily_prelim$temp

  base_temp <- rep(balancepoint_temp, length(temp_daily))

  HDD <- base_temp - temp_daily
  HDD[HDD < 0 ] <- 0

  CDD <- temp_daily - base_temp
  CDD[CDD < 0 ] <- 0

  temp_daily_prelim <-  cbind(temp_daily_prelim, HDD, CDD)

  temp_monthly <- temp_daily_prelim %>%
    mutate(time = lubridate::floor_date(days, "month"))

  days <- temp_monthly %>%
    count(time) %>%
    rename(cdays = n)

  temp_monthly <- temp_monthly %>%
    group_by(time) %>%
    summarize(temp = mean(temp),
              HDD = sum(HDD),
              CDD = sum(CDD)) %>%
    mutate(days = days[[2]]) %>%
    mutate(HDDperday = HDD / days) %>%
    mutate(CDDperday = CDD / days)

  temp_monthly <- temp_monthly %>%
    select(-days)


  # Eload Data
    nterval_eload <- difftime(eload_data$time[2], eload_data$time[1], units = "min")

    if (nterval_eload == 15) {
      data_interval_eload <- "15-min"
    } else if (nterval_eload == 60) {
      data_interval_eload <- "Hourly"
    } else if (nterval_eload < 60) {
      data_interval_eload <- "less than 60"
    } else if (nterval_eload == 1440) {
      data_interval_eload <- "Daily"
    } else if (nterval_eload > 2880) {
      data_interval_eload <- "Monthly"
    }


    if (data_interval_eload == "15-min" | data_interval_eload == "less than 60" |
        data_interval_eload == "Hourly" | data_interval_eload == "Daily") {

      eload_dts_2 <- lubridate::floor_date(eload_data$time, "day")
      eload_data$days <- eload_dts_2
      days <- unique(eload_data$days)
      n_days <- length(days)

      ncolumns <- 2

      eload_daily <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))

      names(eload_daily) <- c("time", "eload")

      eload_daily$time <- days

      for (k in 1:n_days){
        k_day <- days[k]
        idx_k_day <- which(eload_data$days == k_day)
        if (data_is_quantity){
          eload_daily$eload[k] <- sum(eload_data$eload[idx_k_day],
                                           na.rm = T)

        } else {
          eload_daily$eload[k] <- mean(eload_data$eload[idx_k_day],
                                                  na.rm = T) * 24

        names(eload_daily) <- c("time", "eload")
        eload_daily$days <-  lubridate::floor_date(eload_daily$time, "day")
        }

      }

    } else if (data_interval_eload == "Monthly") {

      eload_daily <- eload_data

    }


    if (data_interval_eload == "15-min" | data_interval_eload == "less than 60" |
        data_interval_eload == "Hourly" | data_interval_eload == "Daily" ) {

      eload_monthly <- eload_daily %>%
        mutate(time = lubridate::floor_date(days, "month"))

     days <- eload_monthly %>%
      count(time) %>%
      rename(cdays = n)

     eload_monthly <- eload_monthly %>%
      group_by(time) %>%
      summarize(eload = sum(eload)) %>%
       mutate(days = days[[2]]) %>%
       mutate(eloadperday = eload / days)

    } else {
      eload_monthly <- eload_daily %>%
          inner_join(days, by = "time") %>%
          mutate(eloadperday = eload / cdays) %>%
          mutate(days = cdays)
      }


  data_monthly <- dplyr::inner_join(eload_monthly, temp_monthly, by = c("time"))


  data_monthly <- data_monthly[complete.cases(data_monthly), ]

  return(data_monthly)
}
