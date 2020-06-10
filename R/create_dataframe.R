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

  # check input classes and formats

  if(! is.POSIXct(eload_data$time)){
    stop("Timestamps in 'eload_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! is.POSIXct(temp_data$time)){
    stop("Timestamps in 'temp_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! is.null(operating_mode_data)){
    if(! is.POSIXct(operating_mode_data$time)){
    stop("Timestamps in 'operating_mode_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! assertive::is_numeric(temp_balancepoint)) {
    stop("temp_balancepoint needs to be a numeric input")
  }

  if(! is.null(start_date)) { #TODO:  change
    if(! is.character(start_date)){
      stop("Enter the start date as a character string in 'mm/dd/yyyy hh:mm' format")
    }
  }

  if(! is.null(end_date)) { #TODO: change
    if(! is.character(end_date)){
      stop("Enter the end date as a character string in 'mm/dd/yyyy hh:mm' format")
    }
  }

  # convert to xts objects
  eload_data <- as.data.frame(eload_data)
  temp_data <- as.data.frame(temp_data)
  if(! is.null(operating_mode_data)){
    operating_mode_data <- as.data.frame(operating_mode_data)
  }

  eload_data_xts <- xts(x = eload_data[, -1], order.by = eload_data[, 1])
  temp_data_xts <- xts(x = temp_data[, -1], order.by = temp_data[, 1])
  if(! is.null(operating_mode_data)){
    operating_mode_data_xts <- xts(x = operating_mode_data[, -1], order.by = operating_mode_data[, 1])
  }

  colnames(eload_data_xts) <- "eload"
  colnames(temp_data_xts) <- "temp"

  # determine data intervals of eload, temp, and operating mode data
  scale_eload <- xts::periodicity(eload_data_xts)['scale']
  scale_temp <- xts::periodicity(temp_data_xts)['scale']
  if(! is.null(operating_mode_data)){
    scale_operating_mode_data <- xts::periodicity(operating_mode_data)['scale']
  }

  determine_data_interval_mins <- function(data_scale_info){

    if(data_scale_info$scale == "minute"){
      data_interval <- 15
    } else if (data_scale_info$scale == "hourly"){
      data_interval <- 60
    } else if (data_scale_info$scale == "daily"){
      data_interval <- 1440
    } else if (data_scale_info$scale == "monthly"){
      data_interval <- 43200
    }

    return(data_interval)
  }

  eload_data_interval <- determine_data_interval_mins(scale_eload)
  temp_data_interval <- determine_data_interval_mins(scale_temp)
  if(! is.null(operating_mode_data)){
    operating_mode_data_interval <- determine_data_interval_mins(scale_operating_mode_data)
  }

  if(! is.null(operating_mode_data)){
    max_data_interval <- max(eload_data_interval, temp_data_interval, operating_mode_data_interval)
  } else {
    max_data_interval <- max(eload_data_interval, temp_data_interval)

  }

  # assign modeling interval
  if (missing(convert_to_data_interval)) {
    nterval <- max_data_interval
  } else if(convert_to_data_interval == "15-min") {
    nterval <- 15
  } else if(convert_to_data_interval == "Hourly") {
    nterval <- 60
  } else if (convert_to_data_interval == "Daily"){
    nterval <- 1440
  } else if (convert_to_data_interval == "Monthly") {
    nterval <- 43200 # using 45 days
  } else {
    stop(paste0("Check spellings: convert_to_data_interval can be specified as one of the following:
                '15-min', 'Hourly', 'Daily', or 'Monthly'."))
  }

  if (temp_data_interval > nterval | eload_data_interval > nterval) {
    stop(paste0("Uploaded datasets' intervals bigger than ", convert_to_data_interval))
  }

  if(! is.null(operating_mode_data)) {
    if (operating_mode_data_interval > nterval) {
      stop(paste0("Uploaded datasets' intervals bigger than ", convert_to_data_interval))
    }
  }

  if (convert_to_data_interval == '15-min' | max_data_interval == 15){

    data_xts <- merge(eload_data_xts, temp_data_xts) %>%
      align.time(n = 900)

  } else if (convert_to_data_interval == 'Hourly' | max_data_interval == 60) {

    sum_eload_data_xts <- period.apply(eload_data_xts$eload, INDEX = endpoints(eload_data_xts, "hours"), FUN = sum) %>%
      align.time(n = 3600)

    mean_temp_data_xts <- period.apply(temp_data_xts$temp, INDEX = endpoints(temp_data_xts, "hours"), FUN = mean) %>%
      align.time(n = 3600)

    data.xts <- merge(sum_eload_data_xts, mean_temp_data_xts)

  } else if (convert_to_data_interval == 'Daily' | max_data_interval == 1440) {

    sum_eload_data_xts <- period.apply(eload_data_xts$eload, INDEX = endpoints(eload_data_xts, "days"), FUN = sum) %>%
      align.time(n = 86400)

    mean_temp_data_xts <- period.apply(temp_data_xts$temp, INDEX = endpoints(temp_data_xts, "days"), FUN = mean) %>%
      align.time(n = 86400)

    data.xts <- merge(sum_eload_data_xts, mean_temp_data_xts)
  }

  else if (convert_to_data_interval == 'Monthly' | max_data_interval == 43200) {

    sum_eload_data_xts <- period.apply(eload_data_xts$eload, INDEX = endpoints(eload_data_xts, "months"), FUN = sum, na.rm = T) %>%
      align.time(n = 2592000)

    mean_temp_data_xts <- period.apply(temp_data_xts$temp, INDEX = endpoints(temp_data_xts, "months"), FUN = mean, na.rm = T) %>%
      align.time(n = 2592000)

    data.xts <- merge(sum_eload_data_xts, mean_temp_data_xts)
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
