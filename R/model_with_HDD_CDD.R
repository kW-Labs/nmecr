#' Heating/Cooling Degree Days algorithms using outside air temperature.
#'
#' \code{This function builds an energy use model using one of three algorithms: HDD-CDD Multivariate Regression, HDD Regression,
#' CDD Regression.}#'
#'
#' @param training_list List with training dataframe and operating mode dataframe. Output from create_dataframe
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

model_with_HDD_CDD <- function(training_list = NULL, model_input_options = NULL){

  if(training_list$chosen_modeling_interval == "Hourly") {
    stop("Error: model_with_HDD_CDD cannot be used with Hourly data.")
  }

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
