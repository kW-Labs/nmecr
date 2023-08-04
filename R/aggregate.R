#' Aggregate energy consumption and temperature data. Can be used to aggregate temperature data only as well.
#'
#' \code{Aggregate energy consumption and temperature data to hourly, daily, or monthly data intervals.
#' This function uses dplyr for aggregation as opposed to xts. As a result, the timestamps are not shifted after aggregation.}
#'

#' @param eload_data A dataframe with energy consumption time series. Column names: "time" and "eload". Allowed time intervals: less-than 15-min, 15-min, hourly, daily, monthly
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: less-than 15-min, 15-min, hourly, daily
#' @param additional_independent_variables An optional dataframe for adding independent variables to the regression. This argument is a replacement for the older 'operating_mode_data' argument.
#' The first column should be titled "time" and all proceeding columns should contain numeric data for each additional independent variable.
#' Allowed time intervals: less-than 15-min, 15-min, hourly, daily
#' @param additional_variable_aggregation A vector with aggregation functions for each of the variables in 'additional_independent_variables'.
#' Usage example: c(sum, median) implies two additional independent variables. The first variable will be summed over the specified data interval
#' and the median of the second variable will be taken over the specified data interval. Permissible aggregation functions: sum, mean, median
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: '15-min', 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#' @param shift_normal_weather A logical indicating whether or not to shift the weather data from end of period reporting to beginning of period reporting.
#' This option is set to false by default, but if the user knows that the rest of their data, such as energy use, reports with timestamps at the beginning of each usage
#' period, setting this option to true will align the weather to the beginning of the period as well.
#' @param start_date A POSIXct indicating the inclusive starting datetime to trim observations of all dataframes to start at. The timezone should
#' match the timezone used in the time columns of all dataframes and the end_date argument (if provided). If the start_date argument is not provided,
#' the function will choose a start date based on the latest beginning time stamp of all dataframes.
#' @param end_date A POSIXct indicating the inclusive ending datetime to trim observations of all dataframes to end at. The timezone should match
#' the timezone used in the time columns of all dataframes and the start_date argument (if provided). If the end_date argument is not provided, the
#' function will choose an end date based on the earliest ending time stamp of all dataframes. When providing monthly energy usage data, the end_date
#' parameter can be used to specify the end of the final usage period. Otherwise, the function will attempt to determine the end date for the final
#' monthly usage period using the median interval of all preceding usage periods.
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with energy consumption data, corresponding temperature data, and (if supplied) additional independent variable data aggregated
#' to the indicated data interval. If energy consumption data is not available, aggregated temperature data is returned.
#' @export
#'

aggregate <- function(eload_data = NULL, temp_data = NULL, convert_to_data_interval = c("Hourly", "Daily", "Monthly"),
                      temp_balancepoint = 65) {

  hour <- temp <- eload <- day <- month <- days <- time <- NULL # No visible binding for global variable

  if(convert_to_data_interval == "Hourly") {

    hourly_temp <- temp_data %>%
      dplyr::mutate(hour = lubridate::floor_date(temp_data$time, "hour")) %>%
      dplyr::group_by("time" = hour) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      stats::na.omit()

    if(! is.null(eload_data)) {

      hourly_eload <- eload_data %>%
        dplyr::mutate(hour = lubridate::floor_date(eload_data$time, "hour")) %>%
        dplyr::group_by("time" = hour) %>%
        dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
        stats::na.omit()

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
      stats::na.omit()

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
        stats::na.omit()


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
      stats::na.omit()

    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0

    daily_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)
    
    # Create a time series of monthly usage intervals to aggregate data to.
    # Keep the start and end dates in tact, but have all other intervals begin
    # on the first of the month.
    intervals <- seq.POSIXt(lubridate::floor_date(start_date, "month"), lubridate::floor_date(end_date, "month"), by = "month")
    intervals[1] <- lubridate::floor_date(start_date, "day")
    # If the end date does not end of the first of a month, then add on an extra element to the intervals vector. Because we will
    # later subtract 1 day from these to make them inclusive date ranges, add 1 for now to compensate.
    if (lubridate::mday(end_date) != 1) intervals <- append(intervals, lubridate::floor_date(end_date, "day") + lubridate::as.duration("1 day"))
    
    monthly_intervals <- data.frame(groupnum = seq(1, length(intervals)-1),
                                    interval_start = intervals[-length(intervals)],
                                    interval_end = intervals[-1] - lubridate::as.duration("1 day"),
                                    days = as.numeric(diff(intervals)))
    
    if(! is.null(eload_data)){
      
      # If eload is at an interval of monthly or more, we'll have to reconstruct the usage intervals so that
      # they line up with when energy was measured.
      if(nterval_eload >= lubridate::as.duration("28 days")){
        
        intervals <- eload_data$time %>%
          lubridate::floor_date("day")
        
        # Check if the end_date is beyond the end of the eload dataframe. When this is the case, add another
        # date to the intervals.
        # Also shift last date forward by 1 day to compensate for subtraction that will occur to all values in a few lines
        if (tail(intervals, n=1) < lubridate::floor_date(end_date, "day")){
          intervals <- append(intervals, lubridate::floor_date(end_date, "day") + lubridate::as.duration("1 day"))
        } else {
          intervals[length(intervals)] <- tail(intervals, n=1) + lubridate::as.duration("1 day")
        }
        
        monthly_intervals <- data.frame(groupnum = seq(1, length(intervals)-1),
                                        interval_start = intervals[-length(intervals)],
                                        interval_end = intervals[-1] - lubridate::as.duration("1 day"),
                                        days = as.numeric(diff(intervals)))
        
        # Because eload is already at monthly+ intervals, no need to perform aggregation
        monthly_eload <- eload_data
        
      } else{
        # When eload is at less than monthly intervals, will need to aggregate up.
        # Can use the same usage intervals calculated before this if block
        monthly_eload <- eload_data %>%
          mutate(time = lubridate::floor_date(time, "day")) %>%
          dplyr::inner_join(
            monthly_intervals,
            by = dplyr::join_by("time" >= "interval_start", "time" <= "interval_end")) %>%
          dplyr::group_by(groupnum) %>%
          dplyr::summarize("eload" = sum(eload, na.rm = T)) %>%
          dplyr::rename(time = groupnum) %>%
          dplyr::mutate(time = monthly_intervals$interval_start)
        
      }
    }
    
    # Summarize temperature data by each usage period using an overlap join
    monthly_temp <- daily_data %>%
      dplyr::mutate(time = lubridate::floor_date(time, "day")) %>%
      dplyr::inner_join(
        monthly_intervals,
        by = dplyr::join_by("time" >= "interval_start", "time" <= "interval_end")) %>%
      dplyr::group_by(groupnum) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T),
                       "HDD" = sum(HDD, na.rm = T),
                       "CDD" = sum(CDD, na.rm = T)) %>%
      dplyr::mutate(days = monthly_intervals$days) %>%
      dplyr::rename(time = groupnum) %>%
      dplyr::mutate(time = monthly_intervals$interval_start) %>%
      dplyr::mutate(HDD_perday = HDD / days) %>%
      dplyr::mutate(CDD_perday = CDD / days) #%>%
    #stats::na.omit()
    
    if(! is.null(additional_independent_variables)){
      
      # Summarize additional variables by each period of the eload data using an overlap join
      monthly_additional_independent_variables <- purrr::map2(.x = additional_variable_names,
                                                              .y = additional_variable_aggregation,
                                                              .f = ~additional_independent_variables %>%
                                                                dplyr::mutate(time = lubridate::floor_date(time, "day")) %>%
                                                                dplyr::inner_join(
                                                                  monthly_intervals,
                                                                  by = dplyr::join_by("time" >= "interval_start", "time" <= "interval_end")) %>%
                                                                dplyr::group_by(groupnum) %>%
                                                                dplyr::summarize_at(.vars = .x,
                                                                                    .funs = .y,
                                                                                    na.rm = T) %>%
                                                                dplyr::rename(time = groupnum) %>%
                                                                dplyr::mutate(time = monthly_intervals$interval_start)) %>%
        purrr::reduce(dplyr::left_join, by = 'time')
      
      # Normalize additional variables by days in month
      normalized_monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
        dplyr::mutate(across(-time, ~./monthly_temp$days, .names = "{col}_perday"), .keep = "unused")
      
      # Add on the normalized variables to the dataframe
      monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
        dplyr::bind_cols(normalized_monthly_additional_independent_variables[-1])
      
    }
  }
}
