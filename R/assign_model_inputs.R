#' Assign inputs for various modeling algorithms within nmecr.
#' Passed in the argument model_input_options for all models in nmecr
#'
#' @param timescale_days Numeric corresponding to the timescale for weighting function - used in demand predictions. Default to NULL for energy predictions.
#' @param has_temp_knots_defined Logical specifying whether the temp_knots are pre-defined or will be calculated by the algorithm. Default: FALSE.
#' If set to FALSE, variables 'equal_tem_segment_points' and 'temp_segments_numeric' are used to calculate the temperature knots.#'
#' @param equal_temp_segment_points  Logical specifying structure of temperature segments: equal number of points vs. equal segment length. Default: TRUE
#' Only used if has_temp_knots_defined is set to FALSE.
#' @param temp_segments_numeric Numeric specifying number of temperature segments. Default: 6
#' Only used if has_temp_knots_defined is set to FALSE.
#' @param temp_knots_value Vector specifying manually defined temperature knots.  Only used if has_temp_knots_defined is set to TRUE.
#' @param initial_breakpoints Vector indicating the initial breakpoints (changepoints) to regress over.
#' @param regression_type Character string indicating the modeling algorithm to run
#' @param occupancy_threshold a fractional value for calculating occupancy schedule of the training dataset
#' @param day_normalized Logical specifying whether the monthly models should be day normalized or not. Default: FALSE (not day-normalized)
#'
#' @return a list specifying the chosen algorithm inputs
#' 
#' @export

assign_model_inputs <- function(timescale_days = NULL,
                             has_temp_knots_defined = FALSE,
                             equal_temp_segment_points = TRUE,
                             temp_segments_numeric = 6,
                             temp_knots_value = c(40, 55, 65, 80, 90),
                             initial_breakpoints = c(50,65),
                             regression_type = c("TOWT", "TOW", "SLR",
                                                 "HDD-CDD Multivariate Regression", "HDD-CDD",
                                                 "HDD Regression", "HDD",
                                                 "CDD Regression", "CDD",
                                                 "Three Parameter Cooling", "3PC",
                                                 "Three Parameter Heating", "3PH",
                                                 "Four Parameter Linear Model", "4P",
                                                 "Five Parameter Linear Model", "5P",
                                                 "Mean"),
                             occupancy_threshold = 0.65,
                             day_normalized = FALSE){

  if (! is.null(timescale_days)) {
    if(! assertive::is_numeric(timescale_days)) {
      stop("Error: timescale_days should either be NULL or a numeric input. Default value: NULL")
    }
  }

  if (! assertive::is_logical(has_temp_knots_defined)) {
    stop("Error: has_temp_knots_defined must be a a logical input. Default: FALSE")
  }

  if (! assertive::is_logical(equal_temp_segment_points)) {
    stop("Error: equal_temp_segment_points must be a a logical input. Default: TRUE")
  }

  if (! assertive::is_numeric(temp_segments_numeric)) {
    stop("Error: temp_segments_numeric must be a numeric input. Default value: 6")
  }

  if (! assertive::is_numeric(temp_knots_value)) {
    stop("Error: temp_knots_value must be a numeric vector input. Default value: c(40, 55, 65, 80, 90)")
  }

  if(has_temp_knots_defined == TRUE && equal_temp_segment_points == TRUE) {
    writeLines("You have set 'has_temp_knots_defined' and 'equal_temp_segment_points' to TRUE. These two variables are mutually exclusive: \n 1. when 'has_temp_knots_defined' is set to TRUE, the temperature knots as defined in 'temp_knots_value' are used \n 2. To use 'equal_temp_segment_points', set 'has_temp_knots_defined' to FALSE. Remember to edit the 'temp_segments_numeric' variable as needed.")
  }


  if (! assertive::is_numeric(initial_breakpoints)) {
    stop("Error: initial_breakpoints must be a numeric vector input. Default value: c(50,65)")
  }

  if(! assertive::is_numeric(occupancy_threshold)){
    stop("Error: occupancy_threshold must be between 0 and 1")
  }

  if(occupancy_threshold < 0  | occupancy_threshold > 1){
    stop("Error: occupancy_threshold must be between 0 and 1")
  }

  out <- list()

  out$timescale_days <- timescale_days
  out$has_temp_knots_defined <- has_temp_knots_defined
  out$equal_temp_segment_points <- equal_temp_segment_points
  out$temp_segments_numeric <- temp_segments_numeric
  out$temp_knots_value <- temp_knots_value
  out$initial_breakpoints <- initial_breakpoints
  out$regression_type <- match.arg(regression_type)
  out$occupancy_threshold <- occupancy_threshold
  out$day_normalized <- day_normalized

  return(out)


}
