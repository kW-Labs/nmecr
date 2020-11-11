#' Generate training or prediction dataframes.
#'
#' \code{This function creates a dataframe, combining eload, temp, and additional variable data.
#'  It assumes that the input data is aligned to the start of a time period and outputs a dataframe aligned to the end of time periods.
#'  NA values are ignored during aggregation.}
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
#'
#' @return a dataframe with energy consumption data, temperature data and additional variable data
#' @export

create_dataframe <- function(eload_data = NULL, temp_data = NULL, operating_mode_data = NULL,
                             additional_independent_variables = NULL, additional_variable_aggregation = c(sum, median),
                             start_date = NULL, end_date = NULL,
                             convert_to_data_interval = c("15-min", "Hourly", "Daily", "Monthly"), temp_balancepoint = 65) {

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

  # check input classes and formats ----

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

  # convert to xts objects ----

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

  # Assigning column names  ----

  colnames(eload_data_xts) <- "eload"
  colnames(temp_data_xts) <- "temp"
  if(! is.null(additional_independent_variables)){
    additional_independent_variables_names <- colnames(additional_independent_variables)[-1]
    colnames(additional_independent_variables_xts) <- additional_independent_variables_names
  }

  # Check if input is less than 15-min data. Not allowing less than 15-min data as of now. May change in the future.

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

  # Determine the input data interval and the aggregation interval based on them as well as based on the argument specification.  ----

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

  # Set up for aggregating the additional independent variables  -----

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

      } else {

        aggregated_df_xts <- xts::period.apply(df_xts, INDEX = xts::endpoints(df_xts, xts_index), FUN = aggregation_function, na.rm = T) %>%
          xts::align.time(n = 60*additional_variable_alignment)
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

   # final merged list needs to have timestamps be the end of the period - TODO: see if this can be replaced with an xts::align.time
    corrected_index <- zoo::index(data_xts) + 60*15
    data_xts <- xts::xts(data_xts, corrected_index)


  } else if (nterval == 60*60) { # if the convert_to_data_interval input is 'Hourly' or max_data_interval == 60*60

    sum_eload_data_xts <- xts::period.apply(eload_data_xts$eload, INDEX = xts::endpoints(eload_data_xts, "hours"), FUN = sum, na.rm = T) %>%
      xts::align.time(n = 60*60)

    mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "hours"), FUN = mean, na.rm = T) %>%
      xts::align.time(n = 60*60)


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

    mean_temp_data_xts <- xts::period.apply(temp_data_xts$temp, INDEX = xts::endpoints(temp_data_xts, "days"), FUN = mean, na.rm = T) %>%
      xts::align.time(n = 60*60*24)

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
       align_to_minutes <- 60 # aligning to 60 minutes because when aggregating to Monthly level, we want to get the rounded hour
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
   } else {
     sum_eload_data_xts <- eload_data_xts # if input as monthly - assumption is that the its aligned to the end of the period
   }

   if(! is.null(temp_alignment)){ # aggregate up if less than monthly

     daily_temp <- temp_data %>%
       dplyr::mutate(day = lubridate::floor_date(temp_data$time, "day")) %>%
       dplyr::group_by("time" = day) %>%
       dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
       na.omit()

     base_temp <- rep(temp_balancepoint, length(daily_temp$time))
     HDD <- base_temp - daily_temp$temp
     HDD[HDD < 0 ] <- 0
     CDD <- daily_temp$temp - base_temp
     CDD[CDD < 0 ] <- 0

     daily_data <- dplyr::bind_cols(daily_temp, "HDD" = HDD, "CDD" = CDD)

     monthly_temp <- data.frame(matrix(ncol = 5, nrow = length(sum_eload_data_xts)))
     names(monthly_temp) <- c("time", "temp", "HDD", "CDD", "days")
     monthly_temp$time <- index(sum_eload_data_xts)# aligning to eload - representing the end of the period

     for (i in 2:length(monthly_temp$time)) {

       monthly_temp$temp[i] <- daily_data %>%
         dplyr::filter(time >= monthly_temp$time[i-1] & time < monthly_temp$time[i]) %>%
         dplyr::summarize("temp" = round(mean(temp),2)) %>%
         as.numeric()

       monthly_temp$HDD[i] <- daily_data %>%
         dplyr::filter(time >= monthly_temp$time[i-1] & time < monthly_temp$time[i]) %>%
         dplyr::summarize("HDD" = round(sum(HDD),2)) %>%
         as.numeric

       monthly_temp$CDD[i] <- daily_data %>%
         dplyr::filter(time >= monthly_temp$time[i-1] & time < monthly_temp$time[i]) %>%
         dplyr::summarize("CDD" = round(sum(CDD),2)) %>%
         as.numeric()

       monthly_temp$days[i] <- difftime(monthly_temp$time[i], monthly_temp$time[i-1], units = "days") %>%
         as.numeric()
     }

     mean_temp_data_xts <-  xts::xts(x = monthly_temp[, -1], order.by = monthly_temp[, 1])

   } else {

      mean_temp_data_xts <- temp_data_xts # if input as monthly - assumption is that the its aligned to the end of the period
      mean_temp_data_xts$days <- lubridate::days_in_month(month(index(mean_temp_data_xts) - 30))
      mean_temp_data_xts$HDD <- (temp_balancepoint - mean_temp_data_xts$temp)*mean_temp_data_xts$days
      mean_temp_data_xts$HDD[mean_temp_data_xts$HDD < 0 ] <- 0
      mean_temp_data_xts$CDD <- (mean_temp_data_xts$temp - temp_balancepoint)*mean_temp_data_xts$days
      mean_temp_data_xts$CDD[mean_temp_data_xts$CDD < 0 ] <- 0

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
    data_xts <- data_xts[zoo::index(data_xts) >= lubridate::mdy_hm(start_date)]
  }

  if(! is.null(end_date)) {

    check_end_date <- suppressWarnings(lubridate::mdy_hm(end_date))
    if(is.na(check_end_date)) {
      end_date <- paste(paste(month(end_date), day(end_date), year(end_date), sep = "/"), paste(hour(end_date), minute(end_date), sep = ":"), sep = " ")
    }

    data_xts <- data_xts[zoo::index(data_xts) <= lubridate::mdy_hm(end_date)]
  }

  df <- timetk::tk_tbl(data = data_xts, preserve_index = TRUE, rename_index = "time") # return a df

  # Normalize df by number of days in period, if df is monthly

  if (nterval == 2592000){

    df_columns <- colnames(df)
    remove <- c("time", "temp", "days")
    df_columns <- df_columns[!df_columns %in% remove]

    normalize_by_days <- function(df_col, days_col) {
      normalized_df_col = df_col/days_col
    }

    normalized_df <- data.frame(matrix(nrow = nrow(df), ncol = length(df_columns)))
    names(normalized_df) <- paste0(df_columns, "_perday")

    for (i in 1:length(df_columns)){
      normalized_df[i] <- normalize_by_days(df_col = df[df_columns[i]], days_col = df$days)
    }

    df <- df %>%
      bind_cols(normalized_df)

  }

  return(df)

}
