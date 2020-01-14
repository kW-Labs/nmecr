#' Occupancy detection algorithm for model_with_TOWT.
#'
#' \code{This function generates occupancy schedules for subsequent use in energy data fitting.}
#'
#' @param interval_of_week Unique indicator for each 15-min interval of the week.
#' @param eload_col eload column of training data.
#' @param temp_col temp column of training data.
#' @param interval_minutes Numeric of length of a Time Of Week interval as input variables. Deafult: 15.
#' @return A matrix with unique times of week along with a 0 or 1, representing occupied and unoccupied modes.
#'

find_occ_unocc <- function(interval_of_week = NULL, eload_col = NULL, temp_col = NULL,
                           interval_minutes=15) {

  uniq_time_of_week <- unique(interval_of_week)
  time_of_week_rows <- length(uniq_time_of_week)

  # Define 'occupied' and 'unoccupied' based on a regression
  # of load on outdoor temperature: times of week that the regression usually
  # underpredicts the load will be called 'occupied', the rest are 'unoccupied'

  temp_col_50 <- temp_col - 50
  temp_col_50[temp_col > 50] <- 0
  temp_col_65 <- temp_col - 65
  temp_col_65[temp_col < 65] <- 0

  amod <- lm(eload_col ~ temp_col_50 + temp_col_65, na.action = na.exclude)

  ok_occ <- rep(0, time_of_week_rows)
  for (row_index in 1 : time_of_week_rows) {
    ok_time_of_week <- interval_of_week == uniq_time_of_week[row_index]
    # if the regression underpredicts the load more than 65% of the time
    # then assume it's an occupied period
    if (sum(residuals(amod)[ok_time_of_week] > 0, na.rm = TRUE) <
        0.65 * sum(ok_time_of_week)) {
      ok_occ[row_index] <- 1
    }
  }

  # change to occ if less than 2 unocc
  if (length(which(ok_occ==0)) < 2) {
    ok_occ[ok_occ==0] <- 1
  }

  return(cbind(uniq_time_of_week, ok_occ))
}
