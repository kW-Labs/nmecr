% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_TOWT_reg.R
\name{fit_TOWT_reg}
\alias{fit_TOWT_reg}
\title{Utility function for model_with_TOWT}
\usage{
fit_TOWT_reg(
  training_data = NULL,
  prediction_data = NULL,
  model_input_options = NULL
)
}
\arguments{
\item{training_data}{Training dataframe and operating mode dataframe. Output from create_dataframe}

\item{prediction_data}{Prediction dataframe and operating mode dataframe. Output from create_dataframe}

\item{model_input_options}{List with model inputs specified using assign_model_inputs}
}
\value{
a list with the following components:
\describe{
  \item{model_occupied}{an lm object generated when using Time-of-Week and Time-of-Week & Temperature algorithms}
  \item{model_unoccupied}{an lm object generated when using TOWT algorithm when the unoccupied period is discernably different from the occupied period}
  \item{training_data}{training dataframe along with the model_fit values}
  \item{prediction_data}{prediction dataframe along with the model prediction values. Only generated when prediction_data is supplied to the algorithm}
}
}
\description{
\code{This function builds an energy use/demand models separated by occupancy and temperature regimens.}
}
