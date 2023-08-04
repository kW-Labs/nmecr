#' Generate training or prediction dataframes.
#'
#' \code{This function creates a dataframe, combining eload, temp, and additional variable data. create_dataframe
#'  is the user friendly wrapper for the aggregate function. By default, it assumes that the input data is aligned
#'  to the start of a time period and outputs a dataframe that is also aligned to the start of time periods. In
#'  cases where you are using normal weather data (such as TMY) that reports its timestamps at the end of each
#'  period, then set shift_normal_weather = TRUE to shift the weather data backwards by one interval to match the
#'  timestamp reporting convention for eload.}
#'
#' @param eload_data A dataframe with energy consumption time series. This dataframe should only be energy consumption data and not demand data.
#' Column names: "time" and "eload". Allowed time intervals: less-than 15-min, 15-min, hourly, daily, monthly. The 'time' column must have Date-Time object values.
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: 15-min, hourly, daily, monthly.
#' The 'time' column must have Date-Time object values.
#' @param additional_independent_variables An optional dataframe for adding independent variables to the regression. This argument is a replacement for the older 'operating_mode_data' argument.
#' The first column should be titled "time" and all proceeding columns should contain numeric data for each additional independent variable.
#' Allowed time intervals: less-than 15-min, 15-min, hourly, daily
#' @param additional_variable_aggregation A vector with aggregation functions for each of the variables in 'additional_independent_variables'.
#' Usage example: c(sum, median) implies two additional independent variables. The first variable will be summed over the specified data interval
#' and the median of the second variable will be taken over the specified data interval. Permissible aggregation functions: sum, mean, median
#' @param start_date  A character string, of the format "mm/dd/yyyy hh:mm", indictating the inclusive start date and time of the intended dataframe
#' @param end_date A character string, of the format "mm/dd/yyyy hh:mm", indictating the inclusive end date and time of the intended dataframe.
#' This parameter can also be used to specify the date for the end of the final reporting period for monthly energy usage data and aggregate 
#' other data accordingly. Otherwise, the function will attempt to guess the end date for the final period to aggregate to.
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: '15-min', 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#' @param shift_normal_weather A logical indicating whether or not to shift the weather data from end of period reporting to beginning of period reporting.
#' This option is set to false by default, but if the user knows that the rest of their data, such as energy use, reports with timestamps at the beginning of each usage
#' period, setting this option to true will align the weather to the beginning of the period as well.
#'
#' @importFrom stats median
#' @importFrom magrittr %>%
#'
#' @return a dataframe with energy consumption data, temperature data and (if supplied) additional variable data at the specified data interval. If only temperature data
#' is supplied, the function will return a dataframe with aggregated temperature data.
#' @export

create_dataframe <- function(eload_data = NULL, temp_data = NULL, operating_mode_data = NULL,
                             additional_independent_variables = NULL, additional_variable_aggregation = c(sum, median, mean),
                             start_date = NULL, end_date = NULL,
                             convert_to_data_interval = c("15-min", "Hourly", "Daily", "Monthly"), temp_balancepoint = 65, shift_normal_weather = FALSE) {
  
  day <- temp <- time <- NULL # No visible binding for global variable
  
  ##############################################################################
  ########################## User input error trapping #########################
  ##############################################################################
  
  if(!missing("operating_mode_data")) {
    warning("'operating_mode_data' has been deprecated and will be discontinued in future releases. Please use 'additional_independent_variables' instead.")
    additional_independent_variables <- operating_mode_data
  }
  
  if(! is.null(additional_independent_variables)){
    additional_variables_names <- colnames(additional_independent_variables)
    additional_variables_count <- length(additional_variables_names) - 1
    if(additional_variables_count > length(additional_variable_aggregation)) {
      stop("Please provide an aggregation function for each of the additional variables input. Use argument 'additional_variable_aggregation' to specify the aggregation functions.")
    }
  } # requires that the user input at least the same number of aggregation functions as the additional variables.
  
  ####################### Check input classes and formats ######################
  
  if(! is.null(eload_data)){
    if(! lubridate::is.POSIXct(eload_data$time)){
      stop("Timestamps in 'eload_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
    }
  }
  
  if(! is.null(temp_data)){
    if(! lubridate::is.POSIXct(temp_data$time)){
      stop("Timestamps in 'temp_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
    }
  }
  
  if(! is.null(additional_independent_variables)){
    if(! lubridate::is.POSIXct(additional_independent_variables$time)){
      stop("Timestamps in 'additional_independent_variables' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
    }
  }
  
  if(! assertive::is_numeric(temp_balancepoint)) {
    stop("temp_balancepoint needs to be a numeric input")
  }
  
  if(! is.null(start_date)) {
    if(! is.character(start_date)){
      stop("Enter the start date as a character string, preferably in the 'mm/dd/yyyy hh:mm' format")
    }
  }
  
  if(! is.null(end_date)) {
    if(! is.character(end_date)){
      stop("Enter the end date as a character string, preferably in the 'mm/dd/yyyy hh:mm' format")
    }
  }
  
  ########################### Check timezone of data ##########################
  # Initialize the named list so that if any of the if blocks fail, item will
  # have the "no data" flag
  tz_list <- list(temp = "no data",
                  eload = "no data",
                  additional_independent_variables = "no data")
  
  if (! is.null(temp_data)) tz_list$temp <- attr(temp_data$time, "tzone")
  
  if (! is.null (eload_data)) tz_list$eload <- attr(eload_data$time, "tzone") 
  
  if (! is.null (additional_independent_variables)){
    tz_list$additional_independent_variables <- attr(additional_independent_variables$time, "tzone")
  }
  
  # Remove all items from the list with no data, meaning there was no data frame
  # to read a timezone from.
  
  tz_list <- tz_list[tz_list != "no data"]
  
  # Stop if any timezones do not match
  if (length(unique(tz_list)) > 1L){
    stop_msg <- paste("The timezones of the input dataframes do not match.", "\n",
                      paste(names(tz_list), ": ", tz_list, sep="", collapse="\n"), "\n", sep="")
    stop(stop_msg)
  }
  
  # Store the shared timezone. Going forward, this is what all date-time objects
  # will be declared with.
  timezone <- unlist(unique(tz_list))
  
  # Parse string dates into date-time objects.
  # The truncated = 3 means up to three time specifiers (hour, minute, and second)
  # can be missing and the function will still parse it to a datetime. Every
  # missing time specify will be replaced with 0s.
  if (! is.null(start_date)) start_date <- lubridate::mdy_hms(start_date, tz = timezone, truncated = 3)
  if (! is.null(end_date)) end_date <- lubridate::mdy_hms(end_date, tz = timezone, truncated = 3)
  
  ##############################################################################
  ##################### Find intervals for each time series ####################
  ##############################################################################
  
  # Weather data
  if (! is.null(temp_data)) {
    nterval_temp <- diff(temp_data$time) %>%
      stats::median(na.rm = T) %>%
      lubridate::as.duration()
  } else nterval_temp <- NULL
  
  # Eload data
  if (! is.null(eload_data)) {
    nterval_eload <- diff(eload_data$time) %>%
      stats::median(na.rm = T) %>%
      lubridate::as.duration()
  } else nterval_eload <- NULL
  
  # Additional independent variable data
  if (! is.null(additional_independent_variables)) {
    nterval_additional_independent_variables <- diff(additional_independent_variables$time) %>%
      stats::median(na.rm = T) %>%
      lubridate::as.duration()
  } else nterval_additional_independent_variables <- NULL
  
  # Find the max interval of all intervals
  max_data_interval <- c(nterval_temp, nterval_eload, nterval_additional_independent_variables) %>%
    max(na.rm = T) %>%
    lubridate::as.duration()
  
  # Assign modeling interval - based on either the user's input or max of uploaded datasets
  if (missing(convert_to_data_interval)) {
    nterval <- max_data_interval
  } else if(convert_to_data_interval == "15-min") {
    if (max_data_interval > lubridate::as.duration("15 minutes")) {
      warning("Uploaded datasets have data intervals of greater than 15 minutes. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- lubridate::as.duration("15 minutes")
    }
  } else if(convert_to_data_interval == "Hourly") {
    if (max_data_interval > lubridate::as.duration("1 hour")) {
      warning("Uploaded datasets have data intervals of greater than 1 hour. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- lubridate::as.duration("1 hour")
    }
  } else if (convert_to_data_interval == "Daily"){
    if (max_data_interval > lubridate::as.duration("1 day")) {
      warning("Uploaded datasets have data intervals of greater than 1 day. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- lubridate::as.duration("1 day")
    }
  } else if (convert_to_data_interval == "Monthly") {
    nterval <- lubridate::as.duration("1 month")
  } else {
    stop(paste0("Check spellings: convert_to_data_interval can be specified as one of the following:
                '15-min', 'Hourly', 'Daily', or 'Monthly'."))
  }
  
  # Assign string for interval, used as an input for the aggregate function
  if (nterval == lubridate::as.duration("15 mins")) {
    nterval_string <- "15-min"
  } else if (nterval <= lubridate::as.duration("1 hour")){
    nterval_string <- "Hourly"
  } else if (nterval <= lubridate::as.duration("1 day")){
    nterval_string <- "Daily"
  } else {
    nterval_string <- "Monthly"
  }
  
  df <- aggregate(eload_data, temp_data, additional_independent_variables,
                  additional_variable_aggregation,
                  start_date = start_date,
                  end_date = end_date,
                  convert_to_data_interval = nterval_string,
                  temp_balancepoint = temp_balancepoint, shift_normal_weather = shift_normal_weather)
  
  
  return(df)
  
}
