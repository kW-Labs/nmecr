#' Heating/Cooling Degree Days algorithms using outside air temperature.
#'
#' \code{This function builds an energy use model using one of three algorithms: HDD-CDD Multivariate Regression (HDD-CDD), HDD Regression (HDD),
#' CDD Regression (CDD).}#'
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model}{an lm object}
#'   \item{training_data}{training dataframe along with the model_fit values}
#'   \item{model_input_options}{model_input_options from the input along with additional model specifications.}
#' }
#'
#' @export

model_with_HDD_CDD <- function(training_data = NULL, model_input_options = NULL){

  training_data <- training_data[complete.cases(training_data), ] # remove any incomplete observations

  nterval <- difftime(training_data$time[2], training_data$time[1], units = "min")

  if (nterval == 15){
    nterval_value <- "15-min"
  } else if (nterval == 60){
    nterval_value <- "Hourly"
  } else if (nterval == 1440) {
    nterval_value <- "Daily"
  } else if (nterval >= 40320) {
    nterval_value <- "Monthly"
  }

  model_input_options$chosen_modeling_interval <- nterval_value

  if(model_input_options$chosen_modeling_interval == "Hourly") {
    stop("Error: model_with_HDD_CDD cannot be used with Hourly data.")
  }

  if (model_input_options$regression_type == "HDD-CDD Multivariate Regression" | model_input_options$regression_type == "HDD-CDD") {

    if (nterval_value == "Monthly") {

      if (model_input_options$day_normalized == TRUE){
        linregress <- lm(eload_perday ~ HDD_perday + CDD_perday, data = training_data)
      } else {
        linregress <- lm(eload ~ HDD + CDD, data = training_data)
      }

    } else if (nterval_value == "Daily") {

    linregress <- lm(eload ~ HDD + CDD, data = training_data)

    }

  } else if (model_input_options$regression_type == "HDD Regression" | model_input_options$regression_type == "HDD") {

    if (nterval_value == "Monthly") {

      if (model_input_options$day_normalized == TRUE){
        linregress <- lm(eload_perday ~ HDD_perday, data = training_data)
      } else {
        linregress <- lm(eload ~ HDD, data = training_data)
      }

    } else if (nterval_value == "Daily") {

      linregress <- lm(eload ~ HDD, data = training_data)
    }


  } else if (model_input_options$regression_type == "CDD Regression" | model_input_options$regression_type == "CDD") {

    if (nterval_value == "Monthly") {

      if (model_input_options$day_normalized == TRUE){
        linregress <- lm(eload_perday ~ CDD_perday, data = training_data)
      } else {
        linregress <- lm(eload ~ CDD, data = training_data)
      }

    } else if (nterval_value == "Daily") {

      linregress <- lm(eload ~ CDD, data = training_data)
    }

  }

  out <- list()
  out$model <- linregress
  if (model_input_options$day_normalized == TRUE) {
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values * training_data$days)
  } else {
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
  }
  out$model_input_options <- model_input_options


  return(out)
}
