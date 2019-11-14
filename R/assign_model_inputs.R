




assign_model_inputs <- function(timescale_days = "NA",
                             interval_minutes = 15,
                             has_temp_knots_defined = FALSE,
                             equal_temp_segment_points = TRUE,
                             temp_segments_numeric = 6,
                             temp_knots_value = c(40, 45, 50, 60, 65, 90),
                             initial_breakpoints = c(50,65),
                             regression_type = c("TOWT", "Time-only", "SLR", "HDD-CDD Multivariate Regression", "HDD Regression", "CDD Regression",
                                                 "Three Parameter Cooling", "Three Parameter Heating", "Four Parameter Linear Model",
                                                 "Five Parameter Linear Model")){

  if (timescale_days != "NA" | ! assertive::is_numeric(timescale_days)) {
    stop("Error: timescale_days should either be "NA" or a numeric input. Default value: "NA"")
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
