#' Aggregate energy/demand consumption and temperature data
#'
#' \code{Aggregate energy/demand consumption and/or temperature data to hourly, daily, or monthly data intervals.}
#'
#' @param eload_data A dataframe with energy/demand consumption time series. Column names: "time" and "eload". Allowed time intervals: less-than 60-mins, hourly, daily, monthly
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: less-than 60-mins, hourly, daily
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#'
#' @return a dataframe with energy consumption data and corresponding temperature data, aggregated to the indicated data interval. If energy consumption data is not available,
#' aggregated temperature data is returned.
#' @export

aggregate <- function(eload_data = NULL, temp_data = NULL, convert_to_data_interval = c("Hourly", "Daily", "Monthly"),
                      temp_balancepoint = 65) {

  if(convert_to_data_interval == "Hourly") {

    hourly_temp <- temp_data %>%
      dplyr::mutate(hour = lubridate::floor_date(temp_data$time, "hour")) %>%
      dplyr::group_by("time" = hour) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    if(! is.null(eload_data)) {

      hourly_eload <- eload_data %>%
        dplyr::mutate(hour = lubridate::floor_date(eload_data$time, "hour")) %>%
        dplyr::group_by("time" = hour) %>%
        dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
        na.omit()

      aggregated_data <- hourly_eload %>%
        dplyr::inner_join(hourly_temp, by = "time") %>%
        dplyr::distinct()

      return(aggregated_data)

    } else {

      return(hourly_temp)

    }

  } else if (convert_to_data_interval == "Daily"){

    daily_temp <- temp_data %>%
      dplyr::mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0

    daily_temp <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)

    if(! is.null(eload_data)) {

      daily_eload <- eload_data %>%
        dplyr::mutate(day = lubridate::floor_date(eload_data$time, "day")) %>%
        dplyr::group_by("time" = day) %>%
        dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
        na.omit()


      aggregated_data <- daily_temp %>%
        dplyr::inner_join(daily_eload, by = "time") %>%
        dplyr::distinct()

      return(aggregated_data)

    } else {

      return(daily_temp)
    }

  } else if (convert_to_data_interval == "Monthly"){

    daily_temp <- temp_data %>%
      dplyr::mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0

    daily_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)

    if (is.null (eload_data)){
      monthly_temp <- daily_data %>%
        dplyr::mutate(month = lubridate::floor_date(daily_data$time, "month")) %>%
        dplyr::group_by("time" = month) %>%
        dplyr::summarize("temp" = mean(temp, na.rm = T),
                         "HDD" = sum(HDD, na.rm = T),
                         "CDD" = sum(CDD, na.rm = T))

      monthly_temp <- monthly_temp %>%
        dplyr::mutate(days = lubridate::days_in_month(monthly_temp$time)) %>%
        dplyr::mutate(HDDperday = HDD / days) %>%
        dplyr::mutate(CDDperday = CDD / days) %>%
        na.omit()

    } else if (! is.null(eload_data)) {

      nterval_eload <- median(diff(as.numeric(eload_data$time)))/60

      if(nterval_eload < 40320) {

        monthly_temp <- daily_data %>%
          dplyr::mutate(month = lubridate::floor_date(daily_data$time, "month")) %>%
          dplyr::group_by("time" = month) %>%
          dplyr::summarize("temp" = mean(temp, na.rm = T),
                    "HDD" = sum(HDD, na.rm = T),
                    "CDD" = sum(CDD, na.rm = T))

        monthly_temp <- monthly_temp %>%
          dplyr::mutate(days = lubridate::days_in_month(monthly_temp$time)) %>%
          dplyr::mutate(HDDperday = HDD / days) %>%
          dplyr::mutate(CDDperday = CDD / days) %>%
          na.omit()

      } else {

        monthly_temp <- data.frame("time" = eload_data$time)

        monthly_temp <- data.frame(matrix(ncol = 5, nrow = length(eload_data$time)))
        names(monthly_temp) <- c("time", "temp", "HDD", "CDD", "days")
        monthly_temp$time <- eload_data$time

        for (i in 2:length(eload_data$time)) {
          monthly_temp$temp[i] <- daily_data %>%
            dplyr::filter(time >= monthly_temp$time[i] & time < monthly_temp$time[i+1]) %>%
            dplyr::summarize("temp" = round(mean(temp),2)) %>%
            as.numeric()

          monthly_temp$HDD[i] <- daily_data %>%
            dplyr::filter(time >= monthly_temp$time[i] & time < monthly_temp$time[i+1]) %>%
            dplyr::summarize("HDD" = round(sum(HDD),2)) %>%
            as.numeric

          monthly_temp$CDD[i] <- daily_data %>%
            dplyr::filter(time >= monthly_temp$time[i] & time < monthly_temp$time[i+1]) %>%
            dplyr::summarize("CDD" = round(sum(CDD),2)) %>%
            as.numeric()

          monthly_temp$days[i] <- difftime(monthly_temp$time[i+1], monthly_temp$time[i], units = "days") %>%
            as.numeric()
        }

        monthly_temp <- monthly_temp[complete.cases(monthly_temp), ] %>%
          dplyr::mutate(HDDperday = HDD / days) %>%
          dplyr::mutate(CDDperday = CDD / days)
      }
    }
      if(! is.null(eload_data)) {

        nterval_eload <- median(diff(as.numeric(eload_data$time)))/60

        if(nterval_eload < 40320) { # using 28 days

          monthly_eload <- eload_data %>%
            dplyr::mutate(month = lubridate::floor_date(eload_data$time, "month")) %>%
            dplyr::group_by("time" = month) %>%
            dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
            na.omit()

        } else {

          monthly_eload <- eload_data
        }

        aggregated_data <- monthly_temp %>%
          dplyr::inner_join(monthly_eload, by = "time") %>%
          dplyr::mutate(eloadperday = eload/days) %>%
          dplyr::distinct()

      return(aggregated_data)

    } else {

      return(monthly_temp)

    }
  }
}
