
aggregate <- function(eload_data = NULL, temp_data = NULL, convert_to_data_interval = c("Hourly", "Daily", "Monthly"),
                      temp_balancepoint = 65) {

  if(convert_to_data_interval == "Hourly") {

    hourly_temp <- temp_data %>%
      dplyr::mutate(hour = lubridate::floor_date(temp_data$time, "hour")) %>%
      dplyr::group_by("time" = hour) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    hourly_eload <- eload_data %>%
      dplyr::mutate(hour = lubridate::floor_date(eload_data$time, "hour")) %>%
      dplyr::group_by("time" = hour) %>%
      dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
      na.omit()

    aggregated_data <- hourly_eload %>%
      dplyr::inner_join(hourly_temp, by = "time") %>%
      dplyr::distinct()

    return(aggregated_data)

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

    daily_eload <- eload_data %>%
      dplyr::mutate(day = lubridate::floor_date(eload_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
      na.omit()


    aggregated_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD) %>%
      dplyr::inner_join(daily_eload, by = "time") %>%
      dplyr::distinct()

    return(aggregated_data)

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

    monthly_eload <- eload_data %>%
      dplyr::mutate(month = lubridate::floor_date(eload_data$time, "month")) %>%
      dplyr::group_by("time" = month) %>%
      dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
      na.omit()

    aggregated_data <- monthly_temp %>%
      dplyr::inner_join(monthly_eload, by = "time") %>%
      dplyr::mutate(eloadperday = eload/days) %>%
      dplyr::distinct()


    return(aggregated_data)
  }
}
