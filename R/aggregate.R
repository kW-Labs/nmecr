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
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with energy consumption data and corresponding temperature data, aggregated to the indicated data interval. If energy consumption data is not available,
#' aggregated temperature data is returned.
#' @export
#'

aggregate <- function(eload_data = NULL, temp_data = NULL, additional_independent_variables = NULL,
                          additional_variable_aggregation = NULL,
                          convert_to_data_interval = c("Hourly", "Daily", "Monthly"),
                          temp_balancepoint = 65, shift_normal_weather = FALSE) {
  
  hour <- temp <- eload <- day <- month <- days <- time <- NULL # No visible binding for global variable
  
  # Shift temp data to beginning of period if shift flag is present
  if(shift_normal_weather == TRUE) {
    
    if(! is.null(temp_data)){
      
      nterval_temp <- diff(temp_data$time) %>%
        stats::median(na.rm = T)
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
  
  ################################# HOURLY DATA ################################
  
  if(convert_to_data_interval == "Hourly") {

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
        dplyr::mutate(days = lubridate::days_in_month(monthly_temp$time)) %>%
        dplyr::mutate(HDD_perday = HDD / days) %>%
        dplyr::mutate(CDD_perday = CDD / days) #%>%
        #stats::na.omit()
      
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
      
      # Normalize additional variables by days in month
      normalized_monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
        dplyr::mutate(across(-time, ~./monthly_temp$days))
      
      base::names(normalized_monthly_additional_independent_variables)[-1] <- paste0(names(monthly_additional_independent_variables)[-1], "_perday")
      
      # Add on the normalized variables to the dataframe
      monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
        dplyr::bind_cols(normalized_monthly_additional_independent_variables[-1])
      
      }
      
    }
      
    if(! is.null(eload_data)){
      
      # When eload data is shorter than monthly
      if (nterval_eload < lubridate::as.duration("28 days")){
        
        monthly_eload <- eload_data %>%
          dplyr::mutate(month = lubridate::floor_date(eload_data$time, "month")) %>%
          dplyr::group_by("time" = month) %>%
          dplyr::summarize("eload" = sum(eload, na.rm = T))
        
      # When eload data is at intervals longer than 28 days (could be monthly or longer)
      } else {
        
        monthly_eload <- eload_data
        
        # Create a data frame of time intervals and group numbers for each energy usage period
        eload_intervals <- data.frame(
          groupnum = seq(1, length(eload_data$time)-1),
          interval_start = eload_data$time[-length(eload_data$time)],
          interval_end = eload_data$time[-1])
        
        # Summarize temperature data by each period of the eload data
        monthly_temp <- daily_data %>%
          dplyr::inner_join(
            eload_intervals,
            by = join_by("time" >= "interval_start", "time" < "interval_end")) %>%
          dplyr::group_by(groupnum) %>%
          dplyr::summarize("temp" = mean(temp, na.rm = T),
                           "HDD" = sum(HDD, na.rm = T),
                           "CDD" = sum(CDD, na.rm = T))
        
        # Add timestamps as start of each interval
        colnames(monthly_temp)[1] <- "time"
        monthly_temp$time <- eload_intervals$interval_start
        
        # Calculate degree days per days in month
        monthly_temp$days <- as.numeric(diff(eload_data$time))
        
        monthly_temp <- monthly_temp[stats::complete.cases(monthly_temp), ] %>%
          dplyr::mutate(HDD_perday = HDD / days) %>%
          dplyr::mutate(CDD_perday = CDD / days) #%>%
        #stats::na.omit()
        
        if(! is.null(additional_independent_variables)){
          
          monthly_additional_independent_variables <- additional_independent_variables %>%
            dplyr::inner_join(
              eload_intervals,
              by = join_by("time" >= "interval_start", "time" < "interval_end")) %>%
            dplyr::group_by(groupnum) %>%
            dplyr::summarize_at(.vars = additional_variable_names,
                                .funs = additional_variable_aggregation,
                                na.rm = T)
          
          colnames(monthly_additional_independent_variables)[1] <- "time"
          monthly_additional_independent_variables$time <- eload_intervals$interval_start
          
          # Set column names and delete extraneous columns created by the summarize_at function
          base::names(monthly_additional_independent_variables)[additional_variable_aggregation_indices] <- additional_variable_names
          monthly_additional_independent_variables <- monthly_additional_independent_variables[, c("time", additional_variable_names)]
          
          # Normalize additional variables by days in month
          normalized_monthly_additional_independent_variables <- monthly_additional_independent_variables %>%
            dplyr::mutate(across(-time, ~./monthly_temp$days))
          
          base::names(normalized_monthly_additional_independent_variables)[-1] <- paste0(names(monthly_additional_independent_variables)[-1], "_perday")
          
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
