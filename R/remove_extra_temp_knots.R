#' Remove temp knots if enough temperature datapoints are not found above the highest knot and/or below the lowest knot
#'
#' \code{This function determines temperature knots in the training dataset.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param dataframe Training or Performance period dataframe, an object that has been created by the function create_dataframe() and add_operating_mode_data()
#' @param num_model_runs a numeric object created by determine_count_of_model_runs()
#' @param timescale_days Numeric correspond to the timescale for weighting function.Default: NULL.
#' Change to improve accuracy of short term models.
#'
#' @return a numeric corresponding to the number of model runs to be computed for TOWT
#'
#' @export
#'
#'
#'

# TODO: confirm that this is working correctly

remove_extra_temp_knots <- function(training_data = NULL, temp_knots = NULL) {

  ok_load <- ! is.na(training_data$eload)

  num_temp_knots <- length(temp_knots)

  check_knots <- TRUE
  while (check_knots) {
    if (sum(training_data$temp[ok_load] > temp_knots[num_temp_knots],
            na.rm = TRUE) < 20) {
      # not enough data above upper knot; throw away that upper knot
      temp_knots <- temp_knots[- num_temp_knots]
      num_temp_knots <- num_temp_knots - 1
      if (num_temp_knots == 1) {
        check_knots <- FALSE
      }
    } else {
      # We have enough data above the upper knot, so need to keep checking
      check_knots <- FALSE
    }
  }

  # Same principle as above, for aomount of data below the lowest knot.
  check_knots <- TRUE
  while (check_knots) {
    if (sum(training_data$temp[ok_load] < temp_knots[1], na.rm = TRUE) < 20) {
      # not enough data below lower knot; throw away that lower knot
      temp_knots <- temp_knots[- 1]
      num_temp_knots <- num_temp_knots - 1
      if (num_temp_knots == 1) {
        # We have to keep one knot, even though we have no data below it.
        check_knots <- FALSE
      }
    } else {
      check_knots <- FALSE # we have sufficient data below the lowest knot
    }
  }

  return(temp_knots)

}
