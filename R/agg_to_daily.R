#' Aggregate data to daily data

#' @param dataframe An nx2 time series dataframe.
#' @param operation A character string identifying the operation to perform while aggregating the data: Mean, Sum.
#' @return The orginal dataframe aggregated to daily data interval
#'
#' @export

agg_to_daily <- function(dataframe, operation){

  names(dataframe) <- c("time", "data")

  dts_2 <- lubridate::floor_date(dataframe$time, "day")
  dataframe$days <- dts_2
  days <- unique(dataframe$days)
  n_days <- length(days)

  ncolumns <- 2

  data_daily <- as.data.frame(matrix(nrow = n_days, ncol = ncolumns))
  names(data_daily) <- c("days", "data")

  data_daily$days <- days

  for (k in 1:n_days){
    k_day <- days[k]
    idx_k_day <- which(dataframe$days == k_day)
    if (operation == "Mean") {
      data_daily$data[k] <- mean(dataframe$data[idx_k_day], na.rm = T)
    } else if (operation == "Sum") {
      data_daily$data[k] <- sum(dataframe$data[idx_k_day], na.rm = T)
    }

    names(data_daily) <- c("time", "data")
  }

  return(data_daily)
}
