#' Generate training or prediction dataframes. NA values are ignored during aggregation.
#'
#'
#' @param eload_data A dataframe with energy consumption time series. This dataframe should only be energy consumption data and not demand data. Column names: "time" and "eload". Allowed time intervals: 15-min, hourly, daily, monthly. The 'time' column must have Date-Time object values.
#' @param temp_data A dataframe with weather time series. Column names: "time" and "temp". Allowed time intervals: 15-min, hourly, daily, monthly. The 'time' column must have Date-Time object values.
#' @param additional_independent_variables An optional dataframe for adding independent variables to the regression. This argument is a replacement for the older 'operating_mode_data' argument.
#' @param additional_variable_aggregation A vector with aggregation functions for each of the variables in 'additional_independent_variables'.
#' Usage example: c(sum, median) implies two additional independent variables. The first variable will be summed over the specified data interval
#' and the median of the second variable will be taken over the specified data interval. Permissible aggregation functions: sum, mean, median
#' @param start_date  A character string, of the format "mm/dd/yyy hh:mm", indictating start date and time of the intended dataframe
#' @param end_date A character string, of the format "mm/dd/yyy hh:mm", indictating end date and time of the intended dataframe
#' @param convert_to_data_interval A character string indicating the time interval to which the dataframe should be aggregated: '15-min', 'Hourly', 'Daily', and 'Monthly'
#' @param temp_balancepoint A numeric indicating the balancepoint for the temp_data dataframe
#' @param timestamps A string indicating whether the timestamps in eload_data and temp_data are the start times or the end times.
#'
#' @return a dataframe with energy consumption data and corresponding temperature data, aggregated to the indicated data interval. All data values are aligned to the period end of the indicated data interval.
#' @export

create_dataframe <- function(eload_data = NULL, temp_data = NULL, operating_mode_data = NULL,
                             additional_independent_variables = NULL, additional_variable_aggregation = c(sum, median),
                             start_date = NULL, end_date = NULL,
                             convert_to_data_interval = c("15-min", "Hourly", "Daily", "Monthly"), temp_balancepoint = 65,
                             timestamps = c("start", "end")) {

  if(!missing("operating_mode_data")) {
    warning("'operating_mode_data' has been deprecated and will be discontinued in future releases. Please use 'additional_independent_variables' instead.")
    additional_independent_variables <- operating_mode_data
  }

  if(! is.null(additional_independent_variables)){
    additional_variables_names <- colnames(additional_independent_variables)
    additional_variables_count <- length(additional_variables_names) - 1
    if(additional_variables_count > length(additional_variable_aggregation)) {
      stop("Please provide an aggregation function for each of the additional variables input. Use argument 'additional_variabl_aggregation' to specify the aggregation functions.")
    }
  } # requires that the user input at least the same number of aggregation functions as the additional variables.

  # check input classes and formats START ----

  if(! lubridate::is.POSIXct(eload_data$time)){
    stop("Timestamps in 'eload_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! lubridate::is.POSIXct(temp_data$time)){
    stop("Timestamps in 'temp_data' are not Date-Time objects. Please use the 'lubridate' package to parse in the timestamps appropriately.")
  }

  if(! is.null(additional_independent_variables)){
    if(! is.POSIXct(additional_independent_variables$time)){
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

  # check input classes and formats END ----

  # convert to xts objects START ----

  eload_data <- as.data.frame(eload_data)
  temp_data <- as.data.frame(temp_data)
  if(! is.null(additional_independent_variables)){
    additional_independent_variables <- as.data.frame(additional_independent_variables)
  }

  eload_data_xts <- xts::xts(x = eload_data[, -1], order.by = eload_data[, 1])
  temp_data_xts <- xts::xts(x = temp_data[, -1], order.by = temp_data[, 1])
  if(! is.null(additional_independent_variables)){
    additional_independent_variables_xts <- xts::xts(x = additional_independent_variables[, -1], order.by = additional_independent_variables[, 1])
  }

  # convert to xts objects END ----

  # Assigning column names START ----

  colnames(eload_data_xts) <- "eload"
  colnames(temp_data_xts) <- "temp"
  if(! is.null(additional_independent_variables)){
    additional_independent_variables_names <- colnames(additional_independent_variables)[-1]
    colnames(additional_independent_variables_xts) <- additional_independent_variables_names
  }

  # Assigning column names END ----

  # Check if input is less than 15-min data. Not allowing less than 15-min data as of now. May change in the future. START ----

  if(xts::periodicity(eload_data_xts)['frequency'] < 15 & xts::periodicity(eload_data_xts)['scale'] == 'minute'){
    stop("Cannot input data with frequency of less than 15-minutes")
  }

  if(xts::periodicity(temp_data_xts)['frequency'] < 15 & xts::periodicity(temp_data_xts)['scale'] == 'minute'){
    stop("Cannot input data with frequency of less than 15-minutes")
  }

  if(! is.null(additional_independent_variables)){
    if(xts::periodicity(additional_independent_variables_xts)['frequency'] < 15 & xts::periodicity(additional_independent_variables_xts)['scale'] == 'minute'){
      stop("Cannot input data with frequency of less than 15-minutes")
    }
  }

  # Check if input is less than 15-min data. Not allowing less than 15-min data as of now. May change in the future. END ----

  # Determine the input data interval and the aggregation interval based on them as well as based on the argument specification. START ----

  # determine data intervals of eload, temp, and operating mode data
  scale_eload <- xts::periodicity(eload_data_xts)['scale']
  scale_temp <- xts::periodicity(temp_data_xts)['scale']
  if(! is.null(additional_independent_variables)){
    scale_additional_independent_variables <- xts::periodicity(additional_independent_variables_xts)['scale']
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
  if(! is.null(additional_independent_variables)){
    additional_independent_variables_interval <- determine_data_interval_sec(scale_additional_independent_variables)
  }

  if(! is.null(additional_independent_variables)){
    max_data_interval <- max(eload_data_interval, temp_data_interval, additional_independent_variables_interval)
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
      warning("Uploaded datasets have data intervals of greater than 1 hour. Aggregating to the highest data interval.")
      nterval <- max_data_interval
    } else {
      nterval <- 60*60
    }
  } else if (convert_to_data_interval == "Daily"){
    if (max_data_interval > 60*60*24) {
      warning("Uploaded datasets have data intervals of greater than 1 day. Aggregating to the highest data interval.")
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

  # Determine the input data interval and the aggregation interval based on them as well as based on the argument specification. END ----

  # Set up for aggregating the additional independent variables START -----

  if(! is.null(additional_independent_variables)) {

    dfs <- list()
    for (i in 1:length(additional_independent_variables_names)){
      dfs[i] <- data.frame(additional_independent_variables[additional_independent_variables_names[i]])
    }

    names(dfs) <- additional_independent_variables_names

    apply_aggregation <- function(df, aggregation_function, nterval){ # Function for aggregating individual columns of the additional_independent_variables input

      if (nterval == 60*60){
        xts_index <- "hours"
      } else if (nterval == 60*60*24) {
        xts_index <- "days"
      } else if (nterval <- 60*60*24*mean(30,31)) {
        xts_index <- "months"
      }

      df <- data.frame(df)
      df <- data.frame("time" = additional_independent_variables$time) %>%
        bind_cols(df)

      df_xts <- xts::xts(x = df[, -1], order.by = df[, 1])

      if(nterval == 60*60 | nterval == 60*60*24) { # hourly or daily

        aggregated_df_xts <- xts::period.apply(df_xts, INDEX = xts::endpoints(df_xts, xts_index), FUN = aggregation_function, na.rm = T) %>%
          xts::align.time(n = nterval)

        if(timestamps == 'end') {
          corrected_index <- zoo::index(aggregated_df_xts) - nterval
          aggregated_df_xts <- xts::xts(aggregated_df_xts, order.by = corrected_index)
        }

      } else {

        if (max_data_interval < 60*60*24*mean(30,31)){ # monthly

          aggregated_df_xts <- xts::period.apply(df_xts, INDEX = xts::endpoints(df_xts, xts_index), FUN = aggregation_function, na.rm = T) %>%
            xts::align.time(n = 60*additional_variable_alignment)

          if(timestamps == 'end') {
            corrected_index <- zoo::index(aggregated_df_xts) - 60*align_to_minutes
            aggregated_df_xts <- xts::xts(aggregated_df_xts, order.by = corrected_index)
          }

        } else {

          aggregated_df_xts <- xts::period.apply(df_xts, INDEX = xts::endpoints(df_xts, xts_index), FUN = aggregation_function, na.rm = T) %>%
            xts::align.time(n = 60*additional_variable_alignment)

          if(timestamps == 'start') {

            corrected_index <- zoo::index(aggregated_df_xts) + nterval
            data_xts <- xts::xts(x = aggregated_df_xts[, -1], order.by = corrected_index)

          }
        }
      }
      return(aggregated_df_xts)
    }
  }

  # Set up for aggregating the additional independent variables END -----

  # Begin aggregation
  if (nterval == 60*15) { # if the convert_to_data_interval input is '15-min' or max_data_interval (max interval based on input datasets) == 15 min

    if(! is.null(additional_independent_variables)){
      data_xts <- xts::merge.xts(eload_data_xts, temp_data_xts, additional_independent_variables_xts)
    } else {
      data_xts <- xts::merge.xts(eload_data_xts, temp_data_xts)
    }

  } else if (nterval == 60*60) { # if the convert_to_data_interval input is 'Hourly' or max_data_interval == 60*60

    sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "hours"), FUN = sum, na.rm = T) %>%
      xts::align.time(n = 60*60)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(sum_eload_data_xts) - 60*60
      sum_eload_data_xts <- xts::xts(sum_eload_data_xts$eload, order.by = corrected_index)
    }

    mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "hours"), FUN = mean, na.rm = T) %>%
      xts::align.time(n = 60*60)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(mean_temp_data_xts) - 60*60
      mean_temp_data_xts <- xts::xts(mean_temp_data_xts$temp, order.by = corrected_index)
    }

    if(! is.null(additional_independent_variables)) {

      aggregated_dfs_xts <- purrr::map2(.x = dfs, .y = additional_variable_aggregation, .f = apply_aggregation, nterval = 60*60) # dfs calculated above

      aggregated_df_xts <-  do.call('cbind', aggregated_dfs_xts)
      names(aggregated_df_xts) <- additional_independent_variables_names

      data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts, aggregated_df_xts)

    } else {

      data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts)

    }

  } else if (nterval == 60*60*24) { # if the convert_to_data_interval input is 'Daily' or max_data_interval == 60*60*24. HDD and CDDs being calculated as well.

    sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "days"), FUN = sum, na.rm = T) %>%
      xts::align.time(n = 60*60*24)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(sum_eload_data_xts) - 60*60*24
      sum_eload_data_xts <- xts::xts(sum_eload_data_xts$eload, order.by = corrected_index)
    }

    mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "days"), FUN = mean, na.rm = T) %>%
      xts::align.time(n = 60*60*24)

    if(timestamps == 'end') {
      corrected_index <- zoo::index(mean_temp_data_xts) - 60*60*24
      mean_temp_data_xts <- xts::xts(mean_temp_data_xts$temp, order.by = corrected_index)
    }

    if(! is.null(additional_independent_variables)) {

      aggregated_dfs_xts <- purrr::map2(.x = dfs, .y = additional_variable_aggregation, .f = apply_aggregation, nterval = 60*60*24)

      aggregated_df_xts <-  do.call('cbind', aggregated_dfs_xts)
      names(aggregated_df_xts) <- additional_independent_variables_names

      data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts, aggregated_df_xts)

    } else {

      data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts)

    }

    data_xts$HDD <- temp_balancepoint - data_xts$temp
    data_xts$HDD[data_xts$HDD < 0 ] <- 0
    data_xts$CDD <- data_xts$temp - temp_balancepoint
    data_xts$CDD[data_xts$CDD < 0 ] <- 0

  } else if (nterval == 60*60*24*mean(30,31)) { # if the convert_to_data_interval input is 'Monthly'

   determine_alignment <- function(nterval) {
     if (nterval == 60*15 | nterval == 60*60) {
       align_to_minutes <- 60 # alinging to 60 minutes because when aggregating to Monthly level, we want to get the rounded hour
     } else if (nterval == 60*60*24){
       align_to_minutes <- 60*24
     }
     else {
       align_to_minutes <- NULL
     }
   }

   eload_alignment <- determine_alignment(eload_data_interval)
   temp_alignment <- determine_alignment(temp_data_interval)
   if(! is.null(additional_independent_variables)) {
     additional_variable_alignment <- determine_alignment(additional_independent_variables_interval)
   }

   if(! is.null(eload_alignment)) { # aggregate up if less than monthly
     sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "months"), FUN = sum, na.rm = T) %>%
       xts::align.time(n = 60*eload_alignment)
     if(timestamps == 'end') {
       corrected_index <- zoo::index(sum_eload_data_xts) - 60*eload_alignment
       sum_eload_data_xts <- xts::xts(sum_eload_data_xts$eload, order.by = corrected_index)
     }
   } else {
     sum_eload_data_xts <- eload_data_xts
   }

   if(! is.null(temp_alignment)){ # aggregate up if less than monthly
     mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "months"), FUN = mean, na.rm = T) %>%
     xts::align.time(n = 60*temp_alignment)
     if(timestamps == 'end') {
       corrected_index <- zoo::index(mean_temp_data_xts) - (60*temp_alignment)
       mean_temp_data_xts <- xts::xts(mean_temp_data_xts$temp, order.by = corrected_index)
     }
   } else {
      mean_temp_data_xts <- temp_data_xts
   }

   if(! is.null(additional_independent_variables)) {
     if(! is.null(additional_variable_alignment)){ # aggregate up if less than monthly
       aggregated_dfs_xts <- purrr::map2(.x = dfs, .y = additional_variable_aggregation, .f = apply_aggregation, nterval = 60*60*24*mean(30,31))
       aggregated_df_xts <-  do.call('cbind', aggregated_dfs_xts)
       names(aggregated_df_xts) <- additional_independent_variables_names
     } else {
       aggregated_df_xts <- additional_independent_variables_xts
     }
     data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts, aggregated_df_xts)
   } else {
    data_xts <- xts::merge.xts(sum_eload_data_xts, mean_temp_data_xts)
   }
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
