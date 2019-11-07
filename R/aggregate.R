
aggregate <- function(eload_data = NULL, temp_data = NULL, convert_to_data_interval = c("Hourly", "Daily", "Monthly"),
                      temp_balancepoint = 65) {

  if(convert_to_data_interval == "Hourly") {

    hourly_temp <- temp_data %>%
      mutate(hour = lubridate::floor_date(temp_data$time, "hour")) %>%
      group_by("time" = hour) %>%
      summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    hourly_eload <- eload_data %>%
      mutate(hour = lubridate::floor_date(eload_data$time, "hour")) %>%
      group_by("time" = hour) %>%
      summarize("eload" = sum(eload, na.rm = T)) %>%
      na.omit()

    aggregated_data <- hourly_eload %>%
      dplyr::inner_join(hourly_temp, by = "time") %>%
      dplyr::distinct()

    return(aggregated_data)

  } else if (convert_to_data_interval == "Daily"){

    daily_temp <- temp_data %>%
      mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
      group_by("time" = day) %>%
      summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0

    daily_eload <- eload_data %>%
      mutate(day = lubridate::floor_date(eload_data$time, "day")) %>%
      group_by("time" = day) %>%
      summarize("eload" = sum(eload, na.rm = T)) %>%
      na.omit()


    aggregated_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD) %>%
      dplyr::inner_join(daily_eload, by = "time") %>%
      dplyr::distinct()

    return(aggregated_data)

  } else if (convert_to_data_interval == "Monthly"){

    daily_temp <- temp_data %>%
      mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
      group_by("time" = day) %>%
      summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0


    daily_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)

    monthly_temp <- daily_data %>%
      mutate(month = lubridate::floor_date(daily_data$time, "month")) %>%
      group_by("time" = month) %>%
      summarize("temp" = mean(temp, na.rm = T),
                "HDD" = sum(HDD, na.rm = T),
                "CDD" = sum(CDD, na.rm = T))

    monthly_temp <- monthly_temp %>%
      mutate(days = lubridate::days_in_month(monthly_temp$time)) %>%
      mutate(HDDperday = HDD / days) %>%
      mutate(CDDperday = CDD / days) %>%
      na.omit()

    monthly_eload <- eload_data %>%
      mutate(month = lubridate::floor_date(eload_data$time, "month")) %>%
      group_by("time" = month) %>%
      summarize("eload" = sum(eload, na.rm = T)) %>%
      na.omit()

    aggregated_data <- monthly_temp %>%
      dplyr::inner_join(monthly_eload, by = "time") %>%
      dplyr::distinct()


    return(aggregated_data)
  }
}
