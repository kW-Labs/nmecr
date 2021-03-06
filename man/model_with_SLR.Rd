% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_with_SLR.R
\name{model_with_SLR}
\alias{model_with_SLR}
\title{Simple Linear Regression using outside air temperature.}
\usage{
model_with_SLR(training_data = NULL, model_input_options = NULL)
}
\arguments{
\item{training_data}{Training dataframe and operating mode dataframe. Output from create_dataframe}

\item{model_input_options}{List with model inputs specified using assign_model_inputs}
}
\value{
a list with the following components:
\describe{
  \item{model}{an lm object}
  \item{training_data}{training dataframe along with the model_fit values}
  \item{model_input_options}{model_input_options from the input along with additional model specifications.}
}
}
\description{
\code{This function builds an energy use model using the simple linear regression algorithm.}
}
