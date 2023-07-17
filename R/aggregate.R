#' Aggregate energy consumption and temperature data. Can be used to aggregate temperature data only as well.
#'
#' \code{Aggregate energy consumption and temperature data to hourly, daily, or monthly data intervals.
#' This function uses dplyr for aggregation as opposed to xts. As a result, the timestamps are not shifted after aggregation.}
#'
#' @param eload_data A dataframe with energy consumption time series. Column names: "time" and "eload". Allowed time intervals: less-than 60-mins, hourly, daily, monthly
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: less-than 60-mins, hourly, daily
#' @param additional_independent_variables An optional dataframe for adding independent variables to the regression. This argument is a replacement for the older 'operating_mode_data' argument.
#' @param additional_variable_aggregation A vector with aggregation functions for each of the variables in 'additional_independent_variables'.
#' Usage example: c(sum, median) implies two additional independent variables. The first variable will be summed over the specified data interval
#' and the median of the second variable will be taken over the specified data interval. Permissible aggregation functions: sum, mean, median
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#' @param shift_normal_weather A logical indicating whether or not to shift the weather data from end of period reporting to beginning of period reporting.
#' This option is set to false by default, but if the user knows that the rest of their data, such as energy use, reports with timestamps at the beginning of each usage
#' period, setting this option to true will align the weather to the beginning of the period as well.
#' @param start_date A POSIXct indicating the starting datetime to trim observations of all dataframes to start at. The timezone should
#' match the timezone used in the time columns of all dataframes and the end_date argument (if provided). If the start_date argument is not provided,
#' the function will choose a start date based on the latest beginning time stamp of all dataframes.
#' @param end_date A POSIXct indicating the ending datetime to trim observations of all dataframes to end at. The timezone should match
#' the timezone used in the time columns of all dataframes and the start_date argument (if provided). If the end_date argument is not provided, the
#' function will choose an end date based on the earliest ending time stamp of all dataframes.
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with energy consumption data and corresponding temperature data, aggregated to the indicated data interval. If energy consumption data is not available,
#' aggregated temperature data is returned.
#' @export
#'

aggregate <- function(eload_data = NULL, temp_data = NULL, additional_independent_variables = NULL,
                      additional_variable_aggregation = NULL,
                      convert_to_data_interval = c("15-min", "Hourly", "Daily", "Monthly"),
                      temp_balancepoint = 65, shift_normal_weather = FALSE,
                      start_date = NULL, end_date = NULL) {
  
  hour <- temp <- eload <- day <- month <- days <- time <- NULL # No visible binding for global variable
  
  # Shift temp data to beginning of period if shift flag is present
  if(shift_normal_weather == TRUE) {
    
    if(! is.null(temp_data)){
      
      nterval_temp <- diff(temp_data$time) %>%
        stats::median(na.rm = T) %>%
        lubridate::as.duration()
      
      temp_data <- temp_data %>%
        dplyr::mutate(time = time - nterval_temp)
      
    }
    
  }
  
  # Find interval of energy data
  if(! is.null(eload_data)){
    
    nterval_eload <- diff(eload_data$time) %>%
      stats::median(na.rm = T) %>%
      lubridate::as.duration()
    
  }
  
  # Store variable names to filter columns. Also grab the sequence of important indices
  if(! is.null(additional_independent_variables)) {
    
    additional_variable_names <- colnames(additional_independent_variables[, -1])
    
    additional_variable_aggregation_indices <- seq(from = 2,
                                                   to = length(additional_variable_names)^2 + 1,
                                                   by = length(additional_variable_names)+1)
    
  }
  
  ################# Trim dataframes to be of equal date ranges #################
  # If the start date isn't defined, then set it as the latest start time of all
  # the dataframes
  if(is.null(start_date)){
    start_date <- max(c(eload_data$time[1], temp_data$time[1], additional_independent_variables$time[1]), na.rm = T)
  }
  
  # If the end date isn't defined, then set it as the earliest end time of all
  # the dataframes
  if(is.null(end_date)){
    end_date <- min(c(tail(eload_data$time, n=1), tail(temp_data$time, n=1), tail(additional_independent_variables$time, n=1)), na.rm = T)
  }
  
  if (! is.null(eload_data)){
    eload_data <- eload_data %>%
      dplyr::filter(time >= start_date, time <= end_date)
  }
  
  if (! is.null(temp_data)){
    temp_data <- temp_data %>%
      dplyr::filter(time >= start_date, time <= end_date)
  }
  
  if (! is.null(additional_independent_variables)){
    additional_independent_variables <- additional_independent_variables %>%
      dplyr::filter(time >= start_date, time <= end_date)
  }
  
  ##############################################################################
  ################################ AGGREGATION #################################
  ##############################################################################
  
  ############################### 15-MINUTE DATA ###############################
  
  if(convert_to_data_interval == "15-min") {
    
    # Aggregation to 15-minute (quarterly) temperature data
    quarterly_temp <- temp_data %>%
      dplyr::mutate(quarter = lubridate::floor_date(temp_data$time, "15 mins")) %>%
      dplyr::group_by("time" = quarter) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) #%>%
    #stats::na.omit()
    
    # Aggregation to quarterly energy data
    if(! is.null(eload_data)) {
      
      quarterly_eload <- eload_data %>%
        dplyr::mutate(quarter = lubridate::floor_date(eload_data$time, "15 mins")) %>%
        dplyr::group_by("time" = quarter) %>%
        dplyr::summarize("eload" = sum(eload, na.rm = T)) #%>%
      #stats::na.omit()
      
    }
    
    # Aggregation to quarterly additional independent variable data
    if(! is.null(additional_independent_variables)) {
      
      quarterly_additional_independent_variables <- additional_independent_variables %>%
        dplyr::mutate(quarter = lubridate::floor_date(additional_independent_variables$time, "15 mins")) %>%
        dplyr::group_by("time" = quarter) %>%
        dplyr::summarize_at(.vars = additional_variable_names,
                            .funs = additional_variable_aggregation,
                            na.rm = T)
      
      # Set column names and delete extraneous columns created by the summarize_at function
      base::names(hourly_additional_independent_variables)[additional_variable_aggregation_indices] <- additional_variable_names
      hourly_additional_independent_variables <- hourly_additional_independent_variables[, c("time", additional_variable_names)]
      
    }
    
    # Joining and returning data
    if (! is.null(eload_data)) {
      
      aggregated_data <- quarterly_eload %>%
        dplyr::full_join(quarterly_temp, by = "time") %>%
        dplyr::distinct()
      
      if (! is.null(additional_independent_variables)) {
        
        aggregated_data <- aggregated_data %>%
          dplyr::full_join(quarterly_additional_independent_variables, by = "time") %>%
          dplyr::distinct()
        
      }
      
      return(aggregated_data)
      
    } else {
      
      return(quarterly_temp)
      
    }
    
    ################################# HOURLY DATA ################################
    
  } else if(convert_to_data_interval == "Hourly") {
    
    # Aggregation to hourly temperature data
    hourly_temp <- temp_data %>%
      dplyr::mutate(hour = lubridate::floor_date(temp_data$time, "hour")) %>%
      dplyr::group_by("time" = hour) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) #%>%
    #stats::na.omit()
    
    # Aggregation to hourly energy data
    if(! is.null(eload_data)) {
      
      hourly_eload <- eload_data %>%
        dplyr::mutate(hour = lubridate::floor_date(eload_data$time, "hour")) %>%
        dplyr::group_by("time" = hour) %>%
        dplyr::summarize("eload" = sum(eload, na.rm = T)) #%>%
      #stats::na.omit()
      
    }
    
    # Aggregation to hourly additional independent variable data
    if(! is.null(additional_independent_variables)) {
      
      hourly_additional_independent_variables <- additional_independent_variables %>%
        dplyr::mutate(hour = lubridate::floor_date(additional_independent_variables$time, "hour")) %>%
        dplyr::group_by("time" = hour) %>%
        dplyr::summarize_at(.vars = additional_variable_names,
                            .funs = additional_variable_aggregation,
                            na.rm = T)
      
      # Set column names and delete extraneous columns created by the summarize_at function
      base::names(hourly_additional_independent_variables)[additional_variable_aggregation_indices] <- additional_variable_names
      hourly_additional_independent_variables <- hourly_additional_independent_variables[, c("time", additional_variable_names)]
      
    }
    
    # Joining and returning data
    if (! is.null(eload_data)) {
      
      aggregated_data <- hourly_eload %>%
        dplyr::full_join(hourly_temp, by = "time") %>%
        dplyr::distinct()
      
      if (! is.null(additional_independent_variables)) {
        
        aggregated_data <- aggregated_data %>%
          dplyr::full_join(hourly_additional_independent_variables, by = "time") %>%
          dplyr::distinct()
        
      }
      
      return(aggregated_data)
      
    } else {
      
      return(hourly_temp)
      
    }
    
    
    ################################## DAILY DATA ################################
    
  } else if (convert_to_data_interval == "Daily"){
    
    # Aggregation to daily temperature data
    daily_temp <- temp_data %>%
      dplyr::mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) #%>%
    #stats::na.omit()
    
    # Heating and cooling degree day calculations
    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0
    
    daily_temp <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)
    
    # Aggregation to daily energy data
    if(! is.null(eload_data)) {
      
      daily_eload <- eload_data %>%
        dplyr::mutate(day = lubridate::floor_date(eload_data$time, "day")) %>%
        dplyr::group_by("time" = day) %>%
        dplyr::summarize("eload" = sum(eload, na.rm = T)) #%>%
      #stats::na.omit()
      
    }
    
    # Aggregation to daily additional independent variable data 
    if(! is.null(additional_independent_variables)) {
      
      daily_additional_independent_variables <- additional_independent_variables %>%
        dplyr::mutate(day = lubridate::floor_date(additional_independent_variables$time, "day")) %>%
        dplyr::group_by("time" = day) %>%
        dplyr::summarize_at(.vars = additional_variable_names,
                            .funs = additional_variable_aggregation,
                            na.rm = T)
      
      # Set column names and delete extraneous columns created by the summarize_at function
      base::names(daily_additional_independent_variables)[additional_variable_aggregation_indices] <- additional_variable_names
      daily_additional_independent_variables <- daily_additional_independent_variables[, c("time", additional_variable_names)]
      
    }
    
    # Joining and returning data
    if(! is.null(eload_data)) {
      
      aggregated_data <- daily_temp %>%
        dplyr::full_join(daily_eload, by = "time") %>%
        dplyr::distinct()
      
      if(! is.null(additional_independent_variables)) {
        
        aggregated_data <- aggregated_data %>%
          dplyr::full_join(daily_additional_independent_variables, by = "time") %>%
          dplyr::distinct()
        
      }
      
      return(aggregated_data)
      
    } else {
      
      return(daily_temp)
      
    }
    
    ########################## MONTHLY DATA AGGREGATION ##########################
    
  } else if (convert_to_data_interval == "Monthly"){
    
    daily_temp <- temp_data %>%
      dplyr::mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) #%>%
    #stats::na.omit()
    
    # Consider making the HDD and CDD calcs a function
    
    # Heating and cooling degree day calculations
    base_temp <- rep(temp_balancepoint, length(daily_temp$time))
    HDD <- base_temp - daily_temp$temp
    HDD[HDD < 0 ] <- 0
    CDD <- daily_temp$temp - base_temp
    CDD[CDD < 0 ] <- 0
    
    daily_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)
    
    # Default case for if there is no eload data provided or the interval is more granular than monthly
    if (is.null (eload_data) | (!is.null(eload_data) & nterval_eload < lubridate::as.duration("28 days"))){
      monthly_temp <- daily_data %>%
        dplyr::mutate(month = lubridate::floor_date(daily_data$time, "month")) %>%
        dplyr::group_by("time" = month) %>%
        dplyr::summarize("temp" = mean(temp, na.rm = T),
                         "HDD" = sum(HDD, na.rm = T),
                         "CDD" = sum(CDD, na.rm = T))
      
      monthly_temp <- monthly_temp %>%
        dplyr::mutate(days = lubridate::days_in_month(monthly_temp$time))
      # Fix the number of days in the first and last month (since data may begin
      # and end mid-month)
      monthly_temp$days[1] <- monthly_temp$days[1] - lubridate::mday(start_date) + 1
      monthly_temp$days[length(monthly_temp$days)] <- mday(end_date)
      monthly_temp <- monthly_temp %>%
        dplyr::mutate(HDD_perday = HDD / days) %>%
        dplyr::mutate(CDD_perday = CDD / days) #%>%
      #stats::na.omit()
      
      # Fix the start date
      monthly_temp$time[1] <- lubridate::floor_date(start_date, "day")
      
      
      # Aggregate additional independent variables
      if (! is.null(additional_independent_variables)){
        
        monthly_additional_independent_variables <- additional_independent_variables %>%
          dplyr::mutate(month = lubridate::floor_date(additional_independent_variables$time, "month")) %>%
          dplyr::group_by("time" = month) %>%
          dplyr::summarize_at(.vars = additional_variable_names,
                              .funs = additional_variable_aggregation,
                              na.rm = T)
        
        # Set column names and delete extraneous columns created by the summarize_at function
        base::names(monthly_additional_independent_variables)[additional_variable_aggregation_indices] <- additional_variable_names
        monthly_additional_independent_variables <- monthly_additional_independent_variables[, c("time", additional_variable_names)]
        
        # Fix the start date
        monthly_additional_independent_variables$time[1] <- lubridate::floor_date(start_date, "day")
        
        # Normalize additional variables by days in month
        normalized_monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
          dplyr::mutate(across(-time, ~./monthly_temp$days, .names = "{col}_perday"), .keep = "unused")
        
        # Add on the normalized variables to the dataframe
        monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
          dplyr::bind_cols(normalized_monthly_additional_independent_variables[-1])
        
      }
      
    }
    
    if(! is.null(eload_data)){
      
      # Round eload interval to the nearest day
      rounded_nterval_eload <- round(nterval_eload/lubridate::as.duration("1 day"))*lubridate::as.duration("1 day")
      
      # When eload data is shorter than monthly
      if (nterval_eload < lubridate::as.duration("28 days")){
        
        # Aggregate eload
        monthly_eload <- eload_data %>%
          dplyr::mutate(month = lubridate::floor_date(eload_data$time, "month")) %>%
          dplyr::group_by("time" = month) %>%
          dplyr::summarize("eload" = sum(eload, na.rm = T))
        
        # Fix the start date
        monthly_eload$time[1] <- lubridate::floor_date(start_date, "day")
        
        # Create a dataframe displaying the start and end dates for each usage interval
        eload_intervals <- data.frame(
          interval_start = monthly_eload$time,
          interval_end = lubridate::floor_date(monthly_eload$time, "month") %m+% months(1) - lubridate::as.duration("1 day")) # shift one month forward
        
        # Fix the end date
        eload_intervals$interval_end[length(eload_intervals$interval_end)] <- lubridate::floor_date(end_date, "day")
        
        # When eload data is at intervals longer than 28 days (could be monthly or longer)
      } else {
        
        monthly_eload <- eload_data
        
        # Create a data frame of time intervals with inclusive start and end dates with
        # corresponding group numbers for each energy usage period.
        # For the final usage period, because no end date is provided, the duration is assumed
        # to be the same as the median interval.
        eload_intervals <- data.frame(
          groupnum = seq(1, length(eload_data$time)),
          interval_start = eload_data$time,
          interval_end = append(eload_data$time[-1], tail(eload_data$time, n=1) + rounded_nterval_eload) - lubridate::as.duration("1 day"))
        
        # Summarize temperature data by each period of the eload data using an overlap join
        monthly_temp <- daily_data %>%
          dplyr::inner_join(
            eload_intervals,
            by = join_by("time" >= "interval_start", "time" <= "interval_end")) %>%
          dplyr::group_by(groupnum) %>%
          dplyr::summarize("temp" = mean(temp, na.rm = T),
                           "HDD" = sum(HDD, na.rm = T),
                           "CDD" = sum(CDD, na.rm = T))
        
        # Add timestamps as start of each interval
        colnames(monthly_temp)[1] <- "time"
        monthly_temp$time <- eload_intervals$interval_start
        
        # Create column for days in each usage period. For the final usage period, because
        # an end date is not known, the number of days is assumed to be the median interval.
        monthly_temp$days <- append(as.numeric(diff(eload_data$time)), rounded_nterval_eload/lubridate::as.duration("1 day"))
        
        monthly_temp <- monthly_temp[stats::complete.cases(monthly_temp), ] %>%
          dplyr::mutate(HDD_perday = HDD / days) %>%
          dplyr::mutate(CDD_perday = CDD / days) #%>%
        #stats::na.omit()
        
        if(! is.null(additional_independent_variables)){
          
          # Summarize additional variables by each period of the eload data using an overlap join
          monthly_additional_independent_variables <- additional_independent_variables %>%
            dplyr::inner_join(
              eload_intervals,
              by = join_by("time" >= "interval_start", "time" <= "interval_end")) %>%
            dplyr::group_by(groupnum) %>%
            dplyr::summarize_at(.vars = additional_variable_names,
                                .funs = additional_variable_aggregation,
                                na.rm = T)
          
          # Add the time column back in place of the group number column
          colnames(monthly_additional_independent_variables)[1] <- "time"
          monthly_additional_independent_variables$time <- eload_intervals$interval_start
          
          # Set column names and delete extraneous columns created by the summarize_at function
          base::names(monthly_additional_independent_variables)[additional_variable_aggregation_indices] <- additional_variable_names
          monthly_additional_independent_variables <- monthly_additional_independent_variables[, c("time", additional_variable_names)]
          
          # Normalize additional variables by days in month
          normalized_monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
            dplyr::mutate(across(-time, ~./monthly_temp$days, .names = "{col}_perday"), .keep = "unused")
          
          # Add on the normalized variables to the dataframe
          monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
            dplyr::bind_cols(normalized_monthly_additional_independent_variables[-1])
          
        }
        
      }
      
    }
    
    
    # Joining and returning data
    if(! is.null(eload_data)) {
      
      aggregated_data <- monthly_temp %>%
        dplyr::full_join(monthly_eload, by = "time") %>%
        dplyr::mutate(eload_perday = eload/days) %>%
        dplyr::mutate(interval_start = eload_intervals$interval_start, .after = "time") %>%
        dplyr::mutate(interval_end = eload_intervals$interval_end, .after = "interval_start") %>%
        dplyr::distinct()
      
      if(! is.null(additional_independent_variables)) {
        
        aggregated_data <- aggregated_data %>%
          dplyr::full_join(monthly_additional_independent_variables, by = "time") %>%
          dplyr::distinct()
        
      }
      
      return(aggregated_data)
      
    } else {
      
      return(monthly_temp)
      
    }
  }
}
