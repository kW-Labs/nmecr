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

agg_to_monthly <- function(eload_data = NULL, temp_data = NULL, temp_balancepoint, data_interval_eload){

  if (data_interval_eload != "Monthly") {

    # aggregate data to daily time interval
    daily_data <- agg_to_daily(eload_data, temp_data, temp_balancepoint)

    # find month corresponding to each timestamp
    monthly_data <- daily_data %>%
      mutate(month = lubridate::floor_date(time, "month"))

    # calulate number of days in each month
    days <- monthly_data %>%
      count(month) %>%
      rename(cdays = n)

    # temp data
    temp_monthly <- monthly_data %>%
      group_by(month) %>%
      summarize(temp = mean(temp),
                HDD = sum(HDD),
                CDD = sum(CDD)) %>%
      mutate(days = days[[2]]) %>%
      mutate(HDDperday = HDD / days) %>%
      mutate(CDDperday = CDD / days)

    temp_monthly <- temp_monthly %>%
      select(-days)

    colnames(temp_monthly) <- c("time", "temp", "HDD", "CDD", "HDDperday", "CDDperday")

  } else { # else calculate daily temperature separately

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

    # find month corresponding to each timestamp
    monthly_temp <- temp_daily %>%
      mutate(month = lubridate::floor_date(time, "month"))

    # calulate number of days in each month
    days <- monthly_temp %>%
      count(month) %>%
      rename(cdays = n)

  # temp data
  temp_monthly <- monthly_temp %>%
    group_by(month) %>%
    summarize(temp = mean(temp),
              HDD = sum(HDD),
              CDD = sum(CDD)) %>%
    mutate(days = days[[2]]) %>%
    mutate(HDDperday = HDD / days) %>%
    mutate(CDDperday = CDD / days)

  temp_monthly <- temp_monthly %>%
    select(-days)

  colnames(temp_monthly) <- c("time", "temp", "HDD", "CDD", "HDDperday", "CDDperday")

  }

  # eload Data
  if (data_interval_eload != "Monthly") {

    eload_monthly <- monthly_data %>%
      group_by(month) %>%
      summarize(eload = sum(eload)) %>%
      mutate(days = days[[2]]) %>%
      mutate(eloadperday = eload / days)

    colnames(eload_monthly) <- c("time", "eload", "days", "eloadperday")

  } else if (data_interval_eload == "Monthly") {

    eload_daily <- eload_data
    days <- as.vector(days_in_month(eload_data$time))
    eload_monthly <- eload_daily %>%
      mutate(days) %>%
      mutate(eloadperday = eload / days)

  }

  monthly_data <- dplyr::inner_join(eload_monthly, temp_monthly, by = "time")


  return(monthly_data)
}
