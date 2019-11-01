#' Generate an energy data model using the Time-of-Week and Temperature algorithm.
#'
#'
#' \code{This function builds an energy use model using two algorithms: TOWT and MW.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param training_data Training period dataframe, an object that has been created by the function create_dataframe
#' @param prediction_data Prediction period dataframe,an object that has been created by the function create_dataframe
#' @param timescale_days Numeric correspond to the timescale for weighting function.Default: NULL.
#' Change to improve accuracy of short term models.
#' @param interval_minutes Numeric for the interval period. Default: 15.
#' @param has_temp_knots_defined Boolean specifying whether the temp_knots are pre-defined or will be calculated by the algorithm
#' @param run_temperature_model Boolean specifying whether temperature should or should not be used in regression modeling.
#' @param equal_temp_segment_points Boolean specifying structure of temperature segments: equal number of points vs. equal segment length
#' @param temp_segments_numeric Numeric for number of temperature segments. Default: 6
#' @param temp_knots_value Vector specifying manually defined temperature knots.
#' @param has_operating_modes Boolean specifying whether the energy use profile has varying operating modes.
#' @param train_operating_mode_data dataframe with indicator variables for the various operating modes present in the model training period.
#' @param pred_operating_mode_data dataframe with indicator variables for the various operating modes present in the mdoel prediction period.
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param data_units energy data's units
#'
#' @return a TOWT_baseline object, which is a list with the following components:
#' \describe{
#'   \item{TOWT_model}{an object that has been created by the function create_TOWT_weighted_reg,
#'    and which correspond to the TOWT model.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls}
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{rsdl}{a data frame containing all residual values}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export

model_with_TOWT <- function(training_list = NULL, prediction_list = NULL, model_input_options = NULL){



  timescale_days <- model_input_options$timescale_days
  interval_minutes <- model_input_options$interval_minutes
  regression_type <- model_input_options$regression_type
  has_temp_knots_defined = model_input_options$has_temp_knots_defined
  equal_temp_segment_points = model_input_options$equal_temp_segment_points
  temp_segments_numeric = model_input_options$temp_segments_numeric
  temp_knots_value = model_input_options$temp_knots_value


  # calculate temperature knots ----
  temp_knots <- calculate_temperature_knots(training_list = training_list, has_temp_knots_defined = has_temp_knots_defined,
                                            temp_knots_value = temp_knots_value, temp_segments_numeric = temp_segments_numeric,
                                            equal_temp_segment_points = equal_temp_segment_points)

  # create weighted regressions as per timescale_days ----
  weighted_regressions <- create_weighted_regressions(training_list = training_list, prediction_list = prediction_list,
                                                      timescale_days = timescale_days, interval_minutes = interval_minutes,
                                                      regression_type = regression_type, temp_knots = temp_knots,
                                                      training_operating_mode_data = training_operating_mode_data,
                                                      prediction_operating_mode_data = prediction_operating_mode_data)

  results <- list()

  if(! is.null(training_list$operating_mode_data)){
    results$training_data <- cbind(training_list$dataframe, training_list$operating_mode_data, "model_fit" = weighted_regressions$final_train_matrix)
  } else {
    results$training_data <- cbind(training_list$dataframe, "model_fit" = weighted_regressions$final_train_matrix)
  }


  if(! is.null(prediction_list)){
    if(! is.null(prediction_list$operating_mode_data)){
      results$prediction_data <- cbind(prediction_list$dataframe, prediction_list$operating_mode_data, "model_predictions" = weighted_regressions$final_pred_matrix)
    } else {
      results$prediction_data <- cbind(prediction_list$dataframe, "model_predictions" = weighted_regressions$final_pred_matrix)
    }
  }

  return(results)



}
