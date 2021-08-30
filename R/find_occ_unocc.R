#' Occupancy detection algorithm for model_with_TOWT.
#'
#' \code{This function generates occupancy schedules for subsequent use in energy data fitting.}
#'
#' @param training_data Training dataframe. Output from create_dataframe
#' @param model_input_options Model input options generated from assign_model_inputs
#' @return A matrix with unique times of week along with a 0 or 1, representing occupied and unoccupied modes.
#'

find_occ_unocc <- function(training_data = NULL, model_input_options = NULL) {

  minute_of_week <- (lubridate::wday(training_data$time) - 1) * 24 * 60 +
    lubridate::hour(training_data$time) * 60 + lubridate::minute(training_data$time)

  interval_of_week <- 1 + floor(minute_of_week / model_input_options$interval_minutes)

  uniq_time_of_week <- unique(interval_of_week)
  time_of_week_rows <- length(uniq_time_of_week)

  # Define 'occupied' and 'unoccupied' based on a regression
  # of load on outdoor temperature: times of week that the regression usually
  # underpredicts the load will be called 'occupied', the rest are 'unoccupied'

  eload_col <- training_data$eload
  temp_col <- training_data$temp

  temp_col_50 <- temp_col - 50
  temp_col_50[temp_col > 50] <- 0
  temp_col_65 <- temp_col - 65
  temp_col_65[temp_col < 65] <- 0

  amod <- stats::lm(eload_col ~ temp_col_50 + temp_col_65, na.action = stats::na.exclude)

  ok_occ <- rep(0, time_of_week_rows)
  for (row_index in 1 : time_of_week_rows) {
    ok_time_of_week <- interval_of_week == uniq_time_of_week[row_index]
    # if the regression underpredicts the load more than 65% of the time
    # then assume it's an occupied period
    if (sum(stats::residuals(amod)[ok_time_of_week] > 0, na.rm = TRUE) >
        model_input_options$occupancy_threshold * sum(ok_time_of_week)) {
      ok_occ[row_index] <- 1
    }
  }

  return(data.frame(cbind(uniq_time_of_week, ok_occ)))


}
