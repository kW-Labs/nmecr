# TODO: Implement a try-catch for errors that are thrown when the energy use profile cannot be modeled using 3PH or 3PC.
#' Generate an energy data model using change point models with outside air temperature.
#'
#' \code{This function builds an energy use model using one of four available change point modeling algorithms.}
#'
#' @param training_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param regression_type Character string indictating the modeling algorithm to run: "Three Parameter Heating", "Three Parameter Cooling",
#' "Four Parameter Linear Model", "Five Parameter Linear Model",
#' @param initial_breakpoints vector indicating the initial breakpoints to regress over. Use only with "Five Parameter Linear Model".
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param data_units energy data's units.
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls.}
#'   \item{CP_model}{an object with parameter coefficients and associated p-values resulting from the CP model.}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{model}{the lm object created within 'model_with_CP'.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export

model_with_CP <- function(training_list = NULL, model_input_options = NULL){

  model_input_options$chosen_modeling_interval <- training_list$chosen_modeling_interval

  dependent_variable <- training_list$dataframe$eload
  independent_variable <- training_list$dataframe$temp

  if (model_input_options$regression_type == "Three Parameter Cooling") {

    dummy_cooling_model <- lm(dependent_variable  ~ 1)
    three_paramter_cooling_model <- segmented::segmented(dummy_cooling_model, seg.Z = ~independent_variable)

    out <- list()
    out$model <- three_paramter_cooling_model
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = three_paramter_cooling_model$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "Three Parameter Heating") {

    dummy_heating_model <- lm(dependent_variable ~ - 1)
    three_paramter_heating_model <- segmented::segmented(dummy_heating_model, seg.Z = ~independent_variable)

    out <- list()
    out$model <- three_paramter_heating_model
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = three_paramter_heating_model$fitted.values)
    out$model_input_options <- model_input_options

  } else if (model_input_options$regression_type == "Four Parameter Linear Model"){

    linear_4P_model <- lm(dependent_variable ~ independent_variable)
    four_paramter_linear_model <- segmented::segmented(linear_4P_model, seg.Z = ~independent_variable)

    out <- list()
    out$model <- four_paramter_linear_model
    out$training_data <- data.frame(training_list$dataframe, "model_fit" = four_paramter_linear_model$fitted.values)
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
   out$training_data <- data.frame(training_list$dataframe, "model_fit" = five_paramter_linear_model$fitted.values)
   out$model_input_options <- model_input_options

  }

    return(out)
  }
