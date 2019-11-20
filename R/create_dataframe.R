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
    stop("Error: temp_balancepoint needs to be a numeric input between")
  }

  if(! is.null(start_date)) {
    if(! is.character(start_date)){
      stop("Error: Enter the start date as a character string in 'mm/dd/yyyy hh:mm' format.")
    }
  }

  if(! is.null(end_date)) {
    if(! is.character(end_date)){
      stop("Error: Enter the end date as a character string in 'mm/dd/yyyy hh:mm' format.")
    }
  }


  out <- list()

  convert_to_data_interval <- match.arg(convert_to_data_interval)

  # allocate ntervals as per intended interval for modeling
  if(convert_to_data_interval == "Hourly") {
    nterval <- 60
  } else if (convert_to_data_interval == "Daily"){
    nterval <- 1440
  } else if (convert_to_data_interval == "Monthly") {
    nterval <- 40320 # using 28 days
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
  operating_mode_data$time <- lubridate_timestamp(operating_mode_data$time)

  # determine data intervals of eload, temp, and operating mode data
  # add operating mode data to the return list if available ----

  nterval_eload <- difftime(eload_data$time[2], eload_data$time[1], units = "min")
  nterval_temp <- difftime(temp_data$time[2], temp_data$time[1], units = "min")

  if (! is.null(operating_mode_data)){

    nterval_operating_mode <- difftime(operating_mode_data$time[2], operating_mode_data$time[1], units = "min")

    if (nterval != nterval_operating_mode){
      stop("Please upload the operating mode dataset with the same time interval as that intended for modeling")
    }

    out$operating_mode_data <- operating_mode_data
  }

  # Aggregation ----
  if (nterval_temp > nterval | nterval_eload > nterval) {
    stop(paste0("Error: Uploaded datasets' intervals bigger than ", convert_to_data_interval))
  }

  convert_to_data_interval <- match.arg(convert_to_data_interval)

  dataframe <- nmecr::aggregate(eload_data = eload_data, temp_data = temp_data,
                         convert_to_data_interval = convert_to_data_interval,
                         temp_balancepoint = temp_balancepoint)

  # Filter dataframe based on start and end dates ----

  if(! is.null(start_date)) {
  dataframe <- dataframe %>%
    dplyr::filter(time >= lubridate::mdy_hm(start_date))
  }

  if(! is.null(end_date)) {
    dataframe <- dataframe %>%
      dplyr::filter(time <= lubridate::mdy_hm(end_date))
  }

  out$dataframe <- dataframe
  out$chosen_modeling_interval <- convert_to_data_interval

  return(out)

}
