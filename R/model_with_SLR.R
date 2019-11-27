#' Simple Linear Regression using outside air temperature.
#'
#' \code{This function builds an energy use model using the simple linear regression algorithm.}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model} {an lm object}
#'   \item{training_data} {training dataframe along with the model_fit values}
#'   \item{model_input_options} {model_input_options from the input along with the chosen modeling algorithm.}
#' }
#'
#' @export


model_with_SLR <- function(training_data = NULL, model_input_options = NULL){

  nterval <- difftime(training_data$time[2], training_data$time[1], units = "min")

  if (nterval == 60){
    nterval_value <- "Hourly"
  } else if (nterval == 1440) {
    nterval_value <- "Daily"
  } else if (nterval >= 40320) {
    nterval_value <- "Monthly"
  }

  model_input_options$chosen_modeling_interval <- nterval_value

  if (nterval_value == "Hourly") {

    dframe <- training_data %>%
      select (-c("time"))

    linregress <- lm(eload ~ ., dframe)

  } else if (nterval_value == "Daily") {

    dframe <- training_data %>%
      select(- c("time", "HDD", "CDD"))

    linregress <- lm(eload ~ ., dframe)

  } else if (nterval_value == "Monthly") {

    linregress <- lm(eload ~ temp, training_data) # monthly data is not regressed using operating mode data

  }

  out <- list()
  out$model <- linregress
  out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
  out$model_input_options <- model_input_options

  return(out)
}
