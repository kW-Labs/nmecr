#' TODO: Error message for when used with hourly data
#' Generate an energy data model using HDD and CDD models with outside air temperature.
#'
#' \code{This function builds an energy use model using linear regression on computed heating degree days, or cooling degree days, or a combination of the two.}
#'
#' @param training_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param regression_type Character string indictating the modeling algorithm to run: "Three Parameter Heating", "Three Parameter Cooling",
#' "Four Parameter Linear Model", "Five Parameter Linear Model",
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param data_units energy data's units
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls.}
#'   \item{HDD_CDD_model}{an object with parameter coefficients and associated p-values resulting from the HDD_CDD model.}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{model}{the lm object created within 'model_with_CP'.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export
#'

model_with_HDD_CDD <- function(training_list = NULL, model_input_options = NULL){

  model_input_options$chosen_modeling_interval <- training_list$chosen_modeling_interval

  if(exists("eloadperday", where = training_list$dataframe)) {
      data_interval <- "Monthly"
  } else {
    data_interval <- "Daily"
  }

  if (model_input_options$regression_type == "HDD-CDD Multivariate Regression") {

    if (data_interval == "Monthly") {

    linregress <- lm(eloadperday ~ HDDperday + CDDperday, data = training_list$dataframe)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

    } else if (data_interval == "Daily") {

    linregress <- lm(eload ~ HDD + CDD, data = training_list$dataframe)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

    }

  } else if (model_input_options$regression_type == "HDD Regression") {

    linregress <- lm(eload ~ HDD, data = training_list$dataframe)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "CDD Regression") {

    linregress <- lm(eload ~ CDD, data = training_list$dataframe)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

  }

  return(out)
}
