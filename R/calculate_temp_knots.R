#' Calculate temperature knots
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

calculate_temp_knots <- function(training_list = NULL, model_input_options = NULL) {

  num_points <- length(training_list$dataframe$time)

  if (model_input_options$has_temp_knots_defined) {

    temp_knots <- temp_knots_value

  } else {

    temp0 <- min(training_list$dataframe$temp, na.rm = TRUE)
    temp1 <- max(training_list$dataframe$temp, na.rm = TRUE)

    delta_temp <- temp1 - temp0

    if (model_input_options$equal_temp_segment_points) {

      temp_segment_width <- num_points / model_input_options$temp_segments_numeric

      temp_points <- floor(sort(length(training_list$dataframe$temp) - temp_segment_width * (0 : model_input_options$temp_segments_numeric)) + 0.001)

      temp_ordered <- sort(training_list$dataframe$temp, decreasing = F)

      temp_knots <- temp_ordered[temp_points]

    } else {

      temp_segment_width <- delta_temp / model_input_options$temp_segments_numeric

      temp_knots <- floor(sort(max(training_list$dataframe$temp) - temp_segment_width *  (0 : model_input_options$temp_segments_numeric)) + 0.001)
    }

    return(temp_knots)
  }

}
