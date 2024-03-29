#' Mean model.
#'
#' \code{This function builds an intercept only energy use model.}
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


model_with_mean <- function(training_data = NULL, model_input_options = NULL){

  training_data <- training_data[stats::complete.cases(training_data), ] # remove any incomplete observations

  nterval <-  stats::median(diff(as.numeric(training_data$time)))/60

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

  if (nterval_value == "Monthly" & model_input_options$day_normalized == TRUE) {
    linregress <- stats::lm(eload_perday ~ 1, training_data)
  } else {
    linregress <- stats::lm(eload ~ 1, training_data)
  }

  out <- list()
  out$model <- linregress
  out$model_stats <- dplyr::bind_cols("Variable" = rownames(summary(linregress)$coeff), as.data.frame(summary(linregress)$coeff))

  if (nterval_value == "Monthly") {
    if (model_input_options$day_normalized == TRUE) {
      out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values * training_data$days)
    } else {
      out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
    }
  } else {
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
  }

  out$model_input_options <- model_input_options

  return(out)
}
