#' Assign inputs for various modeling algorithms within nmecr.
#'
#'
#' @param timescale_days Numeric corresponding to the timescale for weighting function - used in demand predictions. Default to NULL for energy predictions.
#' @param interval_minutes Numeric of length of a Time Of Week interval as input variables. Default: 15
#' @param has_temp_knots_defined Logical specifying whether the temp_knots are pre-defined or will be calculated by the algorithm. Default: FALSE
#' @param equal_temp_segment_points  Logical specifying structure of temperature segments: equal number of points vs. equal segment length. Default: TRUE
#' @param temp_segments_numeric Numeric specifying number of temperature segments. Default: 6
#' @param temp_knots_value Vector specifying manually defined temperature knots.
#' @param initial_breakpoints Vector indicating the initial breakpoints (changepoints) to regress over.
#' @param regression_type Character string indictating the modeling algorithm to run:
#'
#' @return a list specifying the chosen algorithm inputs
#'
#' @export

assign_model_inputs <- function(timescale_days = NULL,
                             interval_minutes = 15,
                             has_temp_knots_defined = FALSE,
                             equal_temp_segment_points = TRUE,
                             temp_segments_numeric = 6,
                             temp_knots_value = c(40, 45, 50, 60, 65, 90),
                             initial_breakpoints = c(50,65),
                             regression_type = c("TOWT", "TOW", "SLR", "HDD-CDD Multivariate Regression", "HDD Regression", "CDD Regression",
                                                 "Three Parameter Cooling", "Three Parameter Heating", "Four Parameter Linear Model",
                                                 "Five Parameter Linear Model")){

  if (! is.null(timescale_days)) {
    if(! assertive::is_numeric(timescale_days)) {
      stop("Error: timescale_days should either be NULL or a numeric input. Default value: NULL")
    }
  }

  if (! assertive::is_numeric(interval_minutes)) {
    stop("Error: interval_minutes must be a numeric input. Default value: 15")
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
    stop("Error: temp_knots_value must be a numeric vector input. Default value: c(40,45,50,60,65,90)")
  }

  if (! assertive::is_numeric(initial_breakpoints)) {
    stop("Error: initial_breakpoints must be a numeric vector input. Default value: c(50,65)")
  }

  out <- list()

  out$timescale_days <- timescale_days
  out$interval_minutes <- interval_minutes
  out$has_temp_knots_defined <- has_temp_knots_defined
  out$equal_temp_segment_points <- equal_temp_segment_points
  out$temp_segments_numeric <- temp_segments_numeric
  out$temp_knots_value <- temp_knots_value
  out$initial_breakpoints <- initial_breakpoints
  out$regression_type <- match.arg(regression_type)

  return(out)


}
