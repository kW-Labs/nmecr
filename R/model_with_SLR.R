#' Simple Linear Regression using outside air temperature.
#'
#' \code{This function builds an energy use model using the simple linear regression algorithm.}
#'
#' @param training_list List with training dataframe and operating mode dataframe. Output from create_dataframe
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


model_with_SLR <- function(training_list = NULL, model_input_options = NULL){

  model_input_options$chosen_modeling_interval <- training_list$chosen_modeling_interval

  dframe <- training_list$dataframe

  if(! is.null(training_list$operating_mode_data)){
    dframe <- dplyr::inner_join(dframe, training_list$operating_mode_data, by = "time")
  }

  dframe <- dframe %>%
    dplyr::select(-"time")

  linregress <- lm(eload ~ temp, dframe)

  out <- list()
  out$model <- linregress
  out$training_data <- data.frame(training_list$dataframe, "model_fit" = linregress$fitted.values)
  out$model_input_options <- model_input_options

  return(out)
}




