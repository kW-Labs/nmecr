% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_savings_and_uncertainty.R
\name{calculate_savings_and_uncertainty}
\alias{calculate_savings_and_uncertainty}
\title{Calculate meter-based savings and associated uncertainty as per ASHARE Guideline 14}
\usage{
calculate_savings_and_uncertainty(
  prediction_df = NULL,
  savings_fraction = 0.1,
  modeled_object = NULL,
  model_summary_statistics = NULL,
  confidence_level = 90
)
}
\arguments{
\item{prediction_df}{Model predictions. Output from calculate_model_predictions}

\item{savings_fraction}{savings fraction value to use in savings uncertainty calculations. Used when prediction_df is set to NULL}

\item{modeled_object}{List with model results. Output from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT.}

\item{model_summary_statistics}{Dataframe with model statistics. Output from calculate_summary_statistics.}

\item{confidence_level}{Numeric corresponding to the confidence level to be used for savings uncertainty calculation}
}
\value{
a list with the following components:
\describe{
  \item{savings_df}{a dataframe with savings achieved for each timestamp.}
  \item{savings_summary_df}{a dataframe with total performance perioenergy use, total adjusted baseline use, total savingsachieved , savings percentage,
  savings uncertainty, savings percentage using 50 percent uncertainty, and the specified confidence level}
}
}
\description{
\code{This function calculates meter-based savings achieved and the associated uncertainty.}
}
