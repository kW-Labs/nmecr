#' Align a timeseries to the start of a period.
#'
#' \code{This function serves as a precursor to create_dataframe().}
#'
#' @param data A dataframe with a timestamp and a variable. Column names: "time" and "data".
#' @param original_alignment A string indicating the original alignment of the input data. Options: 'start', 'end'.
#'
#' @return a dataframe with data aligned to the start of the period
#' @export

align_data <- function(data = NULL, original_alignment = NULL) {

  if(is.null(data)) {
    stop("Please input data.")
  }

  if(is.null(original_alignment)) {
    stop("Please specify the original alignment of the data. Options: 'start', 'end'.")
  }

  if (original_alignment == 'start'){
    aligned_data <- data
  } else if (original_alignment == 'end') {

    data <- as.data.frame(data)
    data_xts <- xts::xts(x = data[, -1], order.by = data[, 1])
    data_periodicity <- xts::periodicity(data_xts)

   if (data_periodicity$scale == "monthly"){
      correction <- 60*60*24*mean(30,31)
      corrected_index <- lubridate::floor_date(zoo::index(data_xts) - correction, "month")
    } else if(data_periodicity$scale == "minute"){
      correction <- 60*data_periodicity$frequency
      corrected_index <- zoo::index(data_xts) - correction
    } else if (data_periodicity$scale == "hourly"){
      correction <- 60*60
      corrected_index <- zoo::index(data_xts) - correction
    } else if (data_periodicity$scale == "daily"){
      correction <- 60*60*24
      corrected_index <- zoo::index(data_xts) - correction
    }

    data_xts <- xts::xts(data_xts, corrected_index)

    aligned_data <- timetk::tk_tbl(data = data_xts, preserve_index = TRUE, rename_index = "time")

  } else {
    stop("original_alignment can one of two: 'start' or 'end'. Please check your input")
  }

  names(aligned_data) <- names(data)

  return(aligned_data)

}
