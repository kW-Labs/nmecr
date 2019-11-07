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

create_dataframe <- function(eload_data = NULL, temp_data = NULL, start_date = NULL, end_date = NULL,
                             convert_to_data_interval = c("Hourly", "Daily", "Monthly"), temp_balancepoint = 65,
                             operating_mode_data = NULL) {

  convert_to_data_interval <- match.arg(convert_to_data_interval)

  if(convert_to_data_interval == "Hourly") {
    nterval <- 60
  } else if (convert_to_data_interval == "Daily"){
    nterval <- 1440
  } else if (convert_to_data_interval == "Monthly") {
    nterval <- 40320 # using 28 days
  }

  # convert timestamps to time objects, if originally found to be of class, 'character
  if (is.character(eload_data$time)) {
    eload_data$time <- lubridate::mdy_hm(eload_data$time)
  }

  if (is.character(temp_data$time)) {
    temp_data$time <- lubridate::mdy_hm(temp_data$time)
  }

# determine data intervals of eload, temp, and operating mode data ----
  nterval_eload <- difftime(eload_data$time[2], eload_data$time[1], units = "min")

  nterval_temp <- difftime(temp_data$time[2], temp_data$time[1], units = "min")

  # Aggregation ----
  if (nterval_temp > nterval | nterval_eload > nterval) {
    stop(paste0("Error: Uploaded datasets' intervals bigger than ", convert_to_data_interval))
  }

  dataframe <- aggregate(eload_data = eload_data, temp_data = temp_data,
                         convert_to_data_interval = convert_to_data_interval,
                         temp_balancepoint = temp_balancepoint)

  # Filter dataframe based on start and end dates ----

  if(! is.null(start_date)) {
  dataframe <- dataframe %>%
    dplyr::filter(time >= mdy_hm(start_date))
  }

  if(! is.null(end_date)) {
    dataframe <- dataframe %>%
      dplyr::filter(time <= mdy_hm(end_date))
  }

  # Add operating mode data ----

  # if no operating mode data provided, return dataframe
  if(is.null(operating_mode_data)){
    return(list(dataframe = dataframe))
  } else {

    nterval_operating_mode <- difftime(operating_mode_data$time[2], operating_mode_data$time[1], units = "min")
    #TODO: move up
    # should not be moved up. this block is only evaluated if operating mode data is provided. Moving these
    # commands up will require multiple if (! is.null()) statements

    # check data intervals of the datasets
    if (convert_to_data_interval != nterval_operating_mode){
      stop("Please upload the operating mode dataset with the same time interval as that intended for modeling")
    }

    out <- list()
    out$dataframe <- dataframe
    out$operating_mode_data <- operating_mode_data %>%

    return(out)
  }


}
