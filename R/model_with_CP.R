#' Changepoint algorithms using outside air temperature.
#'
#' \code{This function builds an energy use model using the four changepoint algorithms: Three Parameter Cooling, Three Parameter Heating,
#' Four Parameter Linear Model, Fice Parameter Linear Model.}
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


model_with_CP <- function(training_data = NULL, model_input_options = NULL){

  nterval <- difftime(training_data$time[2], training_data$time[1], units = "min")

  if (nterval == 60){
    nterval_value <- "Hourly"
  } else if (nterval == 1440) {
    nterval_value <- "Daily"
  } else if (nterval >= 40320) {
    nterval_value <- "Monthly"
  }

  model_input_options$chosen_modeling_interval <- nterval_value

  dependent_variable <- training_data$eload
  independent_variable <- training_data$temp

  if (model_input_options$regression_type == "Three Parameter Cooling") {

    dummy_cooling_model <- lm(dependent_variable  ~ 1)
    three_paramter_cooling_model <- segmented::segmented(dummy_cooling_model, seg.Z = ~independent_variable)

    out <- list()
    out$model <- three_paramter_cooling_model
    out$training_data <- data.frame(training_data, "model_fit" = three_paramter_cooling_model$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "Three Parameter Heating") {

    dummy_heating_model <- lm(dependent_variable ~ - 1)
    three_paramter_heating_model <- segmented::segmented(dummy_heating_model, seg.Z = ~independent_variable)

    out <- list()
    out$model <- three_paramter_heating_model
    out$training_data <- data.frame(training_data, "model_fit" = three_paramter_heating_model$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "Four Parameter Linear Model"){

    linear_4P_model <- lm(dependent_variable ~ independent_variable)
    four_paramter_linear_model <- segmented::segmented(linear_4P_model, seg.Z = ~independent_variable)

    out <- list()
    out$model <- four_paramter_linear_model
    out$training_data <- data.frame(training_data, "model_fit" = four_paramter_linear_model$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "Five Parameter Linear Model") {

    linear_5P_model <- lm(dependent_variable ~ independent_variable)
    validate(
      need(! is.null(min(model_input_options$initial_breakpoints)), ""),
      need(! is.null(max(model_input_options$initial_breakpoints)), ""),
      need(min(model_input_options$initial_breakpoints) > min(independent_variable), "Changepoint 1 is lower than the minimum temperature value available in the data"),
      need(max(model_input_options$initial_breakpoints) < max(independent_variable), "Changepoint 2 is higher than the maximum temperature value available in the data")
    )

   if.false <- F
   while (if.false == F){
     tryCatch ({
       five_paramter_linear_model <- segmented::segmented(linear_5P_model, seg.Z = ~independent_variable, psi = model_input_options$initial_breakpoints)
       if.false <- T
       }, error = function(e){
       }, finally = {})
   }

   out <- list()
   out$model <- five_paramter_linear_model
   out$training_data <- data.frame(training_data, "model_fit" = five_paramter_linear_model$fitted.values)
   out$model_input_options <- model_input_options

  }

    return(out)
  }
