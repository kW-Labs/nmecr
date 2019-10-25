#' Generate training or prediction dataframe.
#'
#'
#' @param eload_data A dataframe with energy consumption time series. Column names: "time" and "eload". Allowed time intervals: less-than 60-mins, hourly, daily, monthly
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: less-than 60-mins, hourly, daily
#' @param training_start_datetime  A character string, of the format "mm/dd/yyy hh:mm", indictating start date and time of the training dataframe
#' @param training_end_datetime A character string, of the format "mm/dd/yyy hh:mm", indictating end date and time of the training dataframe
#' @param convert_to_data_interval A character string indicating the time interval to which the training dataframe should be aggregated
#'
#' @return a dataframe with energy consumption data and corresponding temperature data 
#' @export
#' 

create_dataframe <- function(eload_data = NULL, temp_data = NULL, start_date = NULL, end_date = NULL, convert_to_data_interval = c("Hourly", "Daily", "Monthly"), temp_balancepoint = 65) {
  
  # convert timestamps to time objects, if originally found to be of class, 'character' 
  if (is.character(eload_data$time)) {
    eload_data$time <- lubridate::mdy_hm(eload_data$time)
  }
  
  if (is.character(temp_data$time)) {
    temp_data$time <- lubridate::mdy_hm(temp_data$time)
  }
  
  # remove incomplete observations from eload and temp datasets
  eload_data <- eload_data[complete.cases(eload_data), ]
  temp_data <- temp_data[complete.cases(temp_data), ]

  # Assign the data interval to which the training dataframe should be aggregated to
  convert_to_data_interval <- match.arg(convert_to_data_interval)
  
  # Evaluate time interval of eload data
  nterval_eload <- difftime(eload_data$time[2], eload_data$time[1], units = "min")
  
  if (nterval_eload < 60) {
    data_interval_eload <- "less than 60-min"
  } else if (nterval_eload == 60) {
    data_interval_eload <- "Hourly"
  } else if (nterval_eload == 1440) {
    data_interval_eload <- "Daily"
  } else if (nterval_eload > 2880) {
    data_interval_eload <- "Monthly"
  }
  
  # Evaluate time interval of temp data
  nterval_temp <- difftime(temp_data$time[2], temp_data$time[1], units = "min")
  
  if (nterval_temp < 60) {
    data_interval_temp <- "less than 60-min"
  } else if (nterval_temp == 60) {
    data_interval_temp <- "Hourly"
  } else if (nterval_temp == 1440) {
    data_interval_temp <- "Daily"
  } else if (nterval_temp > 2880) {
    data_interval_temp <- "Monthly"
  }
  
  # Aggregation
  if (convert_to_data_interval == "Hourly") {
    
    if(data_interval_eload == "Daily" | data_interval_eload == "Monthly" |
       data_interval_temp == "Daily" | data_interval_temp == "Monthly") {
      stop("The uploaded datasets have a time interval of greater than 1 hour, so cannot be aggregated to hourly")
    }
    
    dataframe <- agg_to_hourly(eload_data, temp_data)
    
  } else if (convert_to_data_interval == "Daily") {
    
    if(data_interval_eload == "Monthly" |
       data_interval_temp == "Monthly") {
      stop("The uploaded datasets have a time interval of greater than 1 day, so cannot be aggregated to daily.")
    }
    
    dataframe <- agg_to_daily(eload_data, temp_data, temp_balancepoint)
  
  } else if (convert_to_data_interval == "Monthly") {

    dataframe <- agg_to_monthly(eload_data, temp_data, temp_balancepoint, data_interval_eload)
  }
  
  # Remove duplicate values
  
  dataframe <- distinct(dataframe)
  
  # Filter dataframe based on start and end dates
  
  if(is.null(start_date) | is.null(end_date)) {
    dataframe <- dataframe
  } else {
    dataframe <- dataframe %>%
      dplyr::filter(time >= lubridate::mdy_hm(start_date)) %>%
      dplyr::filter(time <= lubridate::mdy_hm(end_date))
  }
  
}
