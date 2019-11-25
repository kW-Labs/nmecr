#' Temperature knots calculation for model_with_TOWT
#'
#' \code{This function determines temperature knots in the training dataset.}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a vector with calculated temperature knots
#'

calculate_temp_knots <- function(training_data = NULL, model_input_options = NULL) {

  # Calculate temperature knots ----

  num_points <- length(training_data$time)

  if (model_input_options$has_temp_knots_defined) {

    temp_knots <- model_input_options$temp_knots_value

  } else {

    temp0 <- min(training_data$temp, na.rm = TRUE)
    temp1 <- max(training_data$temp, na.rm = TRUE)

    delta_temp <- temp1 - temp0

    if (model_input_options$equal_temp_segment_points) {

      temp_segment_width <- num_points / model_input_options$temp_segments_numeric

      temp_points <- floor(sort(length(training_data$temp) - temp_segment_width * (0 : model_input_options$temp_segments_numeric)) + 0.001)

      temp_ordered <- sort(training_data$temp, decreasing = F)

      temp_knots <- temp_ordered[temp_points]

    } else {

      temp_segment_width <- delta_temp / model_input_options$temp_segments_numeric

      temp_knots <- floor(sort(max(training_data$temp) - temp_segment_width *  (0 : model_input_options$temp_segments_numeric)) + 0.001)
    }

  }

  # Remove extra temperature knots ----
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
