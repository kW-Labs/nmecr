% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_model_inputs.R
\name{assign_model_inputs}
\alias{assign_model_inputs}
\title{Assign inputs for various modeling algorithms within nmecr.
Passed in as the argument model_input_options for all models in nmecr}
\usage{
assign_model_inputs(
  timescale_days = NULL,
  has_temp_knots_defined = FALSE,
  equal_temp_segment_points = TRUE,
  temp_segments_numeric = 6,
  temp_knots_value = c(40, 55, 65, 80, 90),
  initial_breakpoints = c(50, 65),
  regression_type = c("TOWT", "TOW", "SLR", "HDD-CDD Multivariate Regression", "HDD-CDD",
    "HDD Regression", "HDD", "CDD Regression", "CDD", "Three Parameter Cooling", "3PC",
    "Three Parameter Heating", "3PH", "Four Parameter Linear Model", "4P",
    "Five Parameter Linear Model", "5P", "Mean"),
  occupancy_threshold = 0.65,
  day_normalized = FALSE
)
}
\arguments{
\item{timescale_days}{Numeric corresponding to the timescale for weighting function - used in demand predictions. Default to NULL for energy predictions.}

\item{has_temp_knots_defined}{Logical specifying whether the temp_knots are pre-defined or will be calculated by the algorithm. Default: FALSE.
If set to FALSE, variables 'equal_tem_segment_points' and 'temp_segments_numeric' are used to calculate the temperature knots.#'}

\item{equal_temp_segment_points}{Logical specifying structure of temperature segments: equal number of points vs. equal segment length. Default: TRUE
Only used if has_temp_knots_defined is set to FALSE.}

\item{temp_segments_numeric}{Numeric specifying number of temperature segments. Default: 6
Only used if has_temp_knots_defined is set to FALSE.}

\item{temp_knots_value}{Vector specifying manually defined temperature knots.  Only used if has_temp_knots_defined is set to TRUE.}

\item{initial_breakpoints}{Vector indicating the initial breakpoints (changepoints) to regress over.}

\item{regression_type}{Character string indicating the modeling algorithm to run}

\item{occupancy_threshold}{a fractional value for calculating occupancy schedule of the training dataset}

\item{day_normalized}{Logical specifying whether the monthly models should be day normalized or not. Default: FALSE (not day-normalized)}
}
\value{
a list specifying the chosen algorithm inputs
}
\description{
Assign inputs for various modeling algorithms within nmecr.
Passed in as the argument model_input_options for all models in nmecr
}
