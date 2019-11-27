#' Heating/Cooling Degree Days algorithms using outside air temperature.
#'
#' \code{This function builds an energy use model using one of three algorithms: HDD-CDD Multivariate Regression, HDD Regression,
#' CDD Regression.}#'
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model} {an lm object}
#'   \item{training_data} {training dataframe along with the model_fit values}
#'   \item{model_input_options} {model_input_options from the input along with the chosen modeling algorithm.}
#' }
#'
#' @export

model_with_HDD_CDD <- function(training_data = NULL, model_input_options = NULL){

  nterval <- difftime(training_data$time[2], training_data$time[1], units = "min")

  if (nterval == 60){
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

  if(exists("eloadperday", where = training_data)) {
      data_interval <- "Monthly"
  } else {
    data_interval <- "Daily"
  }

  if (model_input_options$regression_type == "HDD-CDD Multivariate Regression") {

    if (data_interval == "Monthly") {

    linregress <- lm(eloadperday ~ HDDperday + CDDperday, data = training_data)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

    } else if (data_interval == "Daily") {

    linregress <- lm(eload ~ HDD + CDD, data = training_data)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

    }

  } else if (model_input_options$regression_type == "HDD Regression") {

    linregress <- lm(eload ~ HDD, data = training_data)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "CDD Regression") {

    linregress <- lm(eload ~ CDD, data = training_data)

    out <- list()
    out$model <- linregress
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
    out$model_input_options <- model_input_options

  }

  return(out)
}
