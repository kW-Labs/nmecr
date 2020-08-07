#' Generate training or prediction dataframes
#'
#'
#' @param eload_data A dataframe with energy/demand consumption time series. Column names: "time" and "eload". Allowed time intervals: 15-min, hourly, daily, monthly. The 'time' column must have Date-Time object values.
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: 15-min, hourly, daily, monthly. The 'time' column must have Date-Time object values.
#' @param operating_mode_data A dataframe with indicator variables for different energy use profiles withtin the stated time period.
#' operating mode data's time interval must either be the maximum of the three uploaded datasets or be the same as that indicated in the parameter 'convert_to_data_interval' as operating mode data cannot be aggreagated.
#' @param start_date  A character string, of the format "mm/dd/yyy hh:mm", indictating start date and time of the intended dataframe
#' @param end_date A character string, of the format "mm/dd/yyy hh:mm", indictating end date and time of the intended dataframe
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: '15-min', 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#' @param timestamps A string indicating whether the timestamps in eload_data and temp_data are the start times or the end times.
#'
#' @return a list with energy consumption data and corresponding temperature data, aggregated to the indicated data interval
#' @export

create_dataframe <- function(eload_data = NULL, temp_data = NULL, operating_mode_data = NULL,
                             start_date = NULL, end_date = NULL,
                             convert_to_data_interval = c("15-min", "Hourly", "Daily", "Monthly"), temp_balancepoint = 65,
                             timestamps = c("start", "end")) {

  # check input classes and formats

  if(! lubridate::is.POSIXct(eload_data$time)){
    stop("Timestamps in 'eload_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! lubridate::is.POSIXct(temp_data$time)){
    stop("Timestamps in 'temp_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! is.null(operating_mode_data)){
    if(! is.POSIXct(operating_mode_data$time)){
    stop("Timestamps in 'operating_mode_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
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

  if(missing(timestamps)){
    stop("Missing Argument: 'timestamps'. Please indicate whether the timestamps represent the 'start' or the 'end' of the time period.")
  }

  # convert to xts objects
  eload_data <- as.data.frame(eload_data)
  temp_data <- as.data.frame(temp_data)
  if(! is.null(operating_mode_data)){
    operating_mode_data <- as.data.frame(operating_mode_data)
  }

  eload_data_xts <- xts::xts(x = eload_data[, -1], order.by = eload_data[, 1])
  temp_data_xts <- xts::xts(x = temp_data[, -1], order.by = temp_data[, 1])
  if(! is.null(operating_mode_data)){
    operating_mode_data_xts <- xts::xts(x = operating_mode_data[, -1], order.by = operating_mode_data[, 1])
  }

  colnames(eload_data_xts) <- "eload"
  colnames(temp_data_xts) <- "temp"
  if(! is.null(operating_mode_data)){
    operating_mode_names <- colnames(operating_mode_data)[-1]
    colnames(operating_mode_data_xts) <- operating_mode_names
  }

  if(xts::periodicity(eload_data_xts)['frequency'] < 15 & xts::periodicity(eload_data_xts)['scale'] == 'minute'){
    stop("Cannot input data with frequency of less than 15-minutes")
  }

  if(xts::periodicity(temp_data_xts)['frequency'] < 15 & xts::periodicity(temp_data_xts)['scale'] == 'minute'){
    stop("Cannot input data with frequency of less than 15-minutes")
  }

  if(! is.null(operating_mode_data)){
    if(xts::periodicity(operating_mode_data_xts)['frequency'] < 15 & xts::periodicity(operating_mode_data_xts)['scale'] == 'minute'){
      stop("Cannot input data with frequency of less than 15-minutes")
    }
  }

  # determine data intervals of eload, temp, and operating mode data
  scale_eload <- xts::periodicity(eload_data_xts)['scale']
  scale_temp <- xts::periodicity(temp_data_xts)['scale']
  if(! is.null(operating_mode_data)){
    scale_operating_mode_data <- xts::periodicity(operating_mode_data_xts)['scale']
  }

  determine_data_interval_sec <- function(data_scale_info){

    if(data_scale_info$scale == "minute"){
      data_interval <- 60*15
    } else if (data_scale_info$scale == "hourly"){
      data_interval <- 60*60
    } else if (data_scale_info$scale == "daily"){
      data_interval <- 60*60*24
    } else if (data_scale_info$scale == "monthly"){
      data_interval <- 60*60*24*mean(30,31)
    }

    return(data_interval)
  }

  eload_data_interval <- determine_data_interval_sec(scale_eload)
  temp_data_interval <- determine_data_interval_sec(scale_temp)
  if(! is.null(operating_mode_data)){
    operating_mode_data_interval <- determine_data_interval_sec(scale_operating_mode_data)
  }

  if(! is.null(operating_mode_data)){
    max_data_interval <- max(eload_data_interval, temp_data_interval, operating_mode_data_interval)
  } else {
    max_data_interval <- max(eload_data_interval, temp_data_interval)

  }

  # assign modeling interval - based on either the user's input or max of uploaded datasets
  if (missing(convert_to_data_interval)) {
    nterval <- max_data_interval
  } else if(convert_to_data_interval == "15-min") {
    if (max_data_interval > 60*15) {
      warning("Uploaded datasets have data intervals of greater than 15 minutes. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- 60*15
    }
  } else if(convert_to_data_interval == "Hourly") {
    if (max_data_interval > 60*60) {
      warning("Uploaded datasets have data intervals of greater than 15 minutes. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- 60*60
    }
  } else if (convert_to_data_interval == "Daily"){
    if (max_data_interval > 60*60*24) {
      warning("Uploaded datasets have data intervals of greater than 15 minutes. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- 60*60*24
    }
  } else if (convert_to_data_interval == "Monthly") {
      nterval <- 60*60*24*mean(30,31)
  } else {
    stop(paste0("Check spellings: convert_to_data_interval can be specified as one of the following:
                '15-min', 'Hourly', 'Daily', or 'Monthly'."))
  }


  # check data interval of operating mode data - has to match modeling interval
  if (! is.null(operating_mode_data)) {
    if(operating_mode_data_interval != nterval) {
      stop(paste0("Please upload an operating mode data file with the data interval ", convert_to_data_interval))
    }
  }


  # begin aggregation
  if (nterval == 60*15) { # if the convert_to_data_interval input is '15-min' or max_data_interval (max interval based on input datasets) == 15 min

    data_xts <- xts::merge.xts(eload_data_xts, temp_data_xts)

  } else if (nterval == 60*60) { # if the convert_to_data_interval input is 'Hourly' or max_data_interval == 60*60

    sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "hours"), FUN = sum) %>%
      xts::align.time(n = 60*60)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(sum_eload_data_xts) - 60*60
      sum_eload_data_xts <- xts::xts(sum_eload_data_xts$eload, order.by = corrected_index)
    }

    mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "hours"), FUN = mean) %>%
      xts::align.time(n = 60*60)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(mean_temp_data_xts) - 60*60
      mean_temp_data_xts <- xts::xts(mean_temp_data_xts$eload, order.by = corrected_index)
    }

    data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts)

  } else if (nterval == 60*60*24) { # if the convert_to_data_interval input is 'Daily' or max_data_interval == 60*60*24. HDD and CDDs being calculated as well.

    sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "days"), FUN = sum) %>%
      xts::align.time(n = 60*60*24)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(sum_eload_data_xts) - 60*60*24
      sum_eload_data_xts <- xts::xts(sum_eload_data_xts$eload, order.by = corrected_index)
    }

    mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "days"), FUN = mean) %>%
      xts::align.time(n = 60*60*24)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(mean_temp_data_xts) - 60*60*24
      mean_temp_data_xts <- xts::xts(mean_temp_data_xts$temp, order.by = corrected_index)
    }

    data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts)

    data_xts$HDD <- temp_balancepoint - data_xts$temp
    data_xts$HDD[data_xts$HDD < 0 ] <- 0
    data_xts$CDD <- data_xts$temp - temp_balancepoint
    data_xts$CDD[data_xts$CDD < 0 ] <- 0

  } else if (nterval == 60*60*24*mean(30,31)) { # if the convert_to_data_interval input is 'Monthly'

    if(max_data_interval < 60*60*24*mean(30,31)) { # if max uploaded datasets' interval is less than monthly - aggregating up

      if (max_data_interval == 60*15 | max_data_interval == 60*60) { # max uploaded datasets' interval: 15 min or 60 minutes
        align_to_minutes <- 60 # alinging to 60 minutes because when aggregating to Monthly level, we want to get the rounded hour
      } else if (max_data_interval == 60*60*24) { # max uploaded datasets' interval: 1 day
        align_to_minutes <- 60*24
      }

      sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "months"), FUN = sum, na.rm = T) %>%
        xts::align.time(n = 60*align_to_minutes)

      if(timestamps == 'end') {
        corrected_index <- zoo::index(sum_eload_data_xts) - 60*align_to_minutes
        sum_eload_data_xts <- xts::xts(sum_eload_data_xts$eload, order.by = corrected_index)
      }

      mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "months"), FUN = mean, na.rm = T) %>%
        xts::align.time(n = 60*align_to_minutes)

      if(timestamps == 'end') {
        corrected_index <- zoo::index(mean_temp_data_xts) - (60*align_to_minutes)
        mean_temp_data_xts <- xts::xts(mean_temp_data_xts$temp, order.by = corrected_index)
      }

      data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts)
      data_xts$days <- lubridate::days_in_month(zoo::index(data_xts))

      data_xts$HDD <- data_xts$days * (temp_balancepoint - data_xts$temp)
      data_xts$HDD[data_xts$HDD < 0 ] <- 0
      data_xts$CDD <- data_xts$days * (data_xts$temp - temp_balancepoint)
      data_xts$CDD[data_xts$CDD < 0 ] <- 0

      data_xts$HDD_per_day <- temp_balancepoint - data_xts$temp
      data_xts$HDD_per_day[data_xts$HDD_per_day < 0 ] <- 0
      data_xts$CDD_per_day <- data_xts$temp - temp_balancepoint
      data_xts$CDD_per_day[data_xts$CDD_per_day < 0 ] <- 0
      data_xts$eload_per_day <- data_xts$eload / data_xts$days

    } else if (max_data_interval == 60*60*24*mean(30,31)) { # max uploaded datasets' interval: 1 month

        if(eload_data_interval == 60*60*24*mean(30,31)) { # eload is monthly - will need to aggregate temp

        monthly_data <- data.frame(matrix(ncol = 4, nrow = length(eload_data$time)))
        names(monthly_data) <- c("time", "temp", "eload", "days")
        monthly_data$time <- eload_data$time
        monthly_data$eload <- eload_data$eload

        for (i in 2:length(monthly_data$time)) {
          monthly_data$temp[i] <- temp_data %>%
            dplyr::filter(time >= monthly_data$time[i-1] & time < monthly_data$time[i]) %>%
            dplyr::summarize("temp" = round(mean(temp, na.rm = T),2)) %>%
            as.numeric()

          monthly_data$days[i] <- difftime(monthly_data$time[i], monthly_data$time[i-1], units = "days") %>%
            as.numeric()
        }

        n_sec <- median(diff(as.numeric(monthly_data$time)))
        t_1 <- monthly_data$time[1] - n_sec

        monthly_data$temp[1] <- temp_data %>%
          dplyr::filter(time >= t_1 & time < monthly_data$time[1]) %>%
          dplyr::summarize("temp" = round(mean(temp, na.rm = T),2)) %>%
          as.numeric()

        monthly_data$days[1] <- difftime(monthly_data$time[1], t_1, units = "days") %>%
          as.numeric()

        data_xts <- xts::xts(x = monthly_data[, -1], order.by = monthly_data[, 1])

        if (timestamps == "start") { # if the input data timestamps represent start time, align the datasets to the next timestamp.
          corrected_index <- zoo::index(data_xts) + (60*60*24*mean(30,31))
          data_xts <- xts::xts(x = monthly_data[, -1], order.by = corrected_index)
        }

        data_xts$HDD <- data_xts$days * (temp_balancepoint - data_xts$temp)
        data_xts$HDD[data_xts$HDD < 0 ] <- 0
        data_xts$CDD <- data_xts$days * (data_xts$temp - temp_balancepoint)
        data_xts$CDD[data_xts$CDD < 0 ] <- 0

        data_xts$HDD_per_day <- temp_balancepoint - data_xts$temp
        data_xts$HDD_per_day[data_xts$HDD_per_day < 0 ] <- 0
        data_xts$CDD_per_day <- data_xts$temp - temp_balancepoint
        data_xts$CDD_per_day[data_xts$CDD_per_day < 0 ] <- 0
        data_xts$eload_per_day <- data_xts$eload / data_xts$days

      } else if (temp_data_interval == 60*60*24*mean(30,31)) { # temp is monthly - will need to aggregate eload

        monthly_data <- data.frame(matrix(ncol = 4, nrow = length(eload_data$time)))
        names(monthly_data) <- c("time", "temp", "eload", "days")
        monthly_data$time <- temp_data$time
        monthly_data$temp <- temp_data$temp

        for (i in 2:length(monthly_data$time)) {
          monthly_data$eload[i] <- eload_data %>%
            dplyr::filter(time >= monthly_data$time[i-1] & time < monthly_data$time[i]) %>%
            dplyr::summarize("temp" = round(sum(eload, na.rm = T),2)) %>%
            as.numeric()

          monthly_data$days[i] <- difftime(monthly_data$time[i], monthly_data$time[i-1], units = "days") %>%
            as.numeric()

        }

        n_sec <- median(diff(as.numeric(monthly_data$time)))
        t_1 <- monthly_data$time[1] - n_sec

        monthly_data$eload[1] <- eload_data %>%
          dplyr::filter(time >= t_1 & time < eload_data$time[1]) %>%
          dplyr::summarize("eload" = round(sum(eload, na.rm = T),2)) %>%
          as.numeric()

        monthly_data$days[2] <- difftime(monthly_data$time[1], t_1, units = "days") %>%
          as.numeric()

        data_xts <- xts::xts(x = monthly_data[, -1], order.by = monthly_data[, 1])

        if (timestamps == "start") { # if the input data timestamps represent start time, align the datasets to the next timestamp.
          corrected_index <- zoo::index(data_xts) + (60*60*24*mean(30,31))
          data_xts <- xts::xts(x = monthly_data[, -1], order.by = corrected_index)
        }

         data_xts$HDD <- data_xts$days * (temp_balancepoint - data_xts$temp)
         data_xts$HDD[data_xts$HDD < 0 ] <- 0
         data_xts$CDD <- data_xts$days * (data_xts$temp - temp_balancepoint)
         data_xts$CDD[data_xts$CDD < 0 ] <- 0

         data_xts$HDD_per_day <- temp_balancepoint - data_xts$temp
         data_xts$HDD_per_day[data_xts$HDD_per_day < 0 ] <- 0
         data_xts$CDD_per_day <- data_xts$temp - temp_balancepoint
         data_xts$CDD_per_day[data_xts$CDD_per_day < 0 ] <- 0
         data_xts$eload_per_day <- data_xts$eload / data_xts$days
      }
    }
  }

  # Add operating mode data
  if (! is.null(operating_mode_data)) {
    data_xts <- xts::merge.xts(data_xts, operating_mode_data_xts)
  }


  # Filter dataframe based on start and end dates ----

  if(! is.null(start_date)) {

    check_start_date <- suppressWarnings(lubridate::mdy_hm(start_date))
    if(is.na(check_start_date)) {
      start_date <- paste(paste(month(start_date), day(start_date), year(start_date), sep = "/"), paste(hour(start_date), minute(start_date), sep = ":"), sep = " ")
    }
    data_xts <- data_xts[zoo::index(data_xts) > lubridate::mdy_hm(start_date)]
  }

  if(! is.null(end_date)) {

    check_end_date <- suppressWarnings(lubridate::mdy_hm(end_date))
    if(is.na(check_end_date)) {
      end_date <- paste(paste(month(end_date), day(end_date), year(end_date), sep = "/"), paste(hour(end_date), minute(end_date), sep = ":"), sep = " ")
    }

    data_xts <- data_xts[zoo::index(data_xts) < lubridate::mdy_hm(end_date)]
  }

  df <- timetk::tk_tbl(data = data_xts, preserve_index = TRUE, rename_index = "time") # return a df

  return(df)

}
