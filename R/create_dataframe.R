#' Generate training or prediction dataframe.
#'
#'
#' @param eload_data A dataframe with energy/demand consumption time series. Column names: "time" and "eload". Allowed time intervals: less-than 60-mins, hourly, daily, monthly
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: less-than 60-mins, hourly, daily
#' @param operating_mode_data A dataframe with indicator variables for different energy use profiles withtin the stated time period.
#' A time column, corresponding to eload_data and/or temp_data must be included.
#' @param start_date  A character string, of the format "mm/dd/yyy hh:mm", indictating start date and time of the intended dataframe
#' @param end_date A character string, of the format "mm/dd/yyy hh:mm", indictating end date and time of the intended dataframe
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#'
#' @return a list with energy consumption data and corresponding temperature data, aggregated to the indicated data interval
#' @export

create_dataframe <- function(eload_data = NULL, temp_data = NULL, operating_mode_data = NULL,
                             start_date = NULL, end_date = NULL,
                             convert_to_data_interval = c("Hourly", "Daily", "Monthly"), temp_balancepoint = 65) {

  if(! assertive::is_numeric(temp_balancepoint)) {
    stop("temp_balancepoint needs to be a numeric input between")
  }

  if(! is.null(start_date)) {
    if(! is.character(start_date)){
      stop("Enter the start date as a character string in 'mm/dd/yyyy hh:mm' format.")
    }
  }

  if(! is.null(end_date)) {
    if(! is.character(end_date)){
      stop("Enter the end date as a character string in 'mm/dd/yyyy hh:mm' format.")
    }
  }

  # convert timestamps to time objects, if originally found to be of class, 'character'
  lubridate_timestamp <- function(time_col) {
    if(is.character(time_col)) {
      time_col <- lubridate::mdy_hm(time_col)
    } else {
      return(time_col)
    }
  }

  eload_data$time <- lubridate_timestamp(eload_data$time)
  temp_data$time <- lubridate_timestamp(temp_data$time)


  # determine data intervals of eload, temp, and operating mode data
  nterval_eload <- difftime(eload_data$time[2], eload_data$time[1], units = "min")
  nterval_temp <- difftime(temp_data$time[2], temp_data$time[1], units = "min")

  nterval_max <- max(nterval_eload, nterval_temp)

  if(! is.null(operating_mode_data)) {
    operating_mode_data$time <- lubridate_timestamp(operating_mode_data$time)
    nterval_operating_mode <- difftime(operating_mode_data$time[2], operating_mode_data$time[1], units = "min")
    nterval_max <- max(nterval_max, nterval_operating_mode)
  }

  # assign modeling interval
  if (missing(convert_to_data_interval)) {
    nterval <- nterval_max
  } else if(convert_to_data_interval == "Hourly") {
    nterval <- 60
  } else if (convert_to_data_interval == "Daily"){
    nterval <- 1440
  } else if (convert_to_data_interval == "Monthly") {
    nterval <- 40320 # using 28 days
  } else {
    stop(paste0("Check spellings: convert_to_data_interval can be specified as one of the following:
                'Hourly', 'Daily', or 'Monthly'."))
  }

  if (nterval_temp > nterval | nterval_eload > nterval) {
    stop(paste0("Uploaded datasets' intervals bigger than ", convert_to_data_interval))
  }

  if(! is.null(operating_mode_data)) {
    if (nterval_operating_mode > nterval) {
      stop(paste0("Uploaded datasets' intervals bigger than ", convert_to_data_interval))
    }
  }

  if(missing(convert_to_data_interval)) {

    if (nterval == 60){
      nterval_value <- "Hourly"
    } else if (nterval == 1440) {
      nterval_value <- "Daily"
    } else if (nterval >= 40320) {
      nterval_value <- "Monthly"
    }

    convert_to_data_interval <- nterval_value
  } else {
    convert_to_data_interval <- convert_to_data_interval
  }


  dataframe <- aggregate(eload_data = eload_data, temp_data = temp_data,
                         convert_to_data_interval = convert_to_data_interval,
                         temp_balancepoint = temp_balancepoint)

  if (! is.null(operating_mode_data)) {
    dataframe <- dataframe %>%
      dplyr::left_join(operating_mode_data, by = "time")
  }

  # Filter dataframe based on start and end dates ----

  if(! is.null(start_date)) {
  dataframe <- dataframe %>%
    dplyr::filter(time >= lubridate::mdy_hm(start_date))
  }

  if(! is.null(end_date)) {
    dataframe <- dataframe %>%
      dplyr::filter(time <= lubridate::mdy_hm(end_date))
  }

  return(dataframe)

}
