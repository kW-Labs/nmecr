#' Time-of-Week and Temperature.
#'
#' \code{This function builds an energy use/demand models using two algorithms: TOWT and MW.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model_occupied} {an lm object generated when using Time-of-Week and TIme-of-Week & Temperature algorithms}
#'   \item{model_unoccupied} {an lm object generated when using TOWT algorithm when the unoccupied period is discernably different from the occupied period}
#'   \item{training_data} {training dataframe along with the model_fit values}
#'   \item{prediction_data} {prediction dataframe along with the model prediction values. Only generated when prediction_list is supplied to the algorithm}
#'   \item{model_input_options} {model_input_options from the input along with the chosen modeling algorithm.}
#' }
#'
#' @export

model_with_TOWT <- function(training_data = NULL, prediction_data = NULL, model_input_options = NULL){

  nterval <- difftime(training_data$time[2], training_data$time[1], units = "min")

  if (nterval == 60){
    nterval_value <- "Hourly"
  } else if (nterval == 1440) {
    nterval_value <- "Daily"
  } else if (nterval >= 40320) {
    nterval_value <- "Monthly"
  }

  model_input_options$chosen_modeling_interval <- nterval_value

  if(model_input_options$chosen_modeling_interval == "Monthly") {
    stop("Error: model_with_TOWT cannot be used with Monthly data.")
  }

  # calculate temperature knots
  model_input_options$calculated_temp_knots <- calculate_temp_knots(training_data = training_data, model_input_options = model_input_options)

  # Run for energy modeling - timescale_days not used
  if (is.null(model_input_options$timescale_days)) {

    # train_weight_vec not essentially needed for energy modeling.
    # it is needed for fit_TOWT_reg however
    # therefore, keeping it as 1 for energy modeling
    model_input_options$train_weight_vec <- rep(1, length(training_data$time))

    # fit linear regression
    reg_out <- fit_TOWT_reg(training_data = training_data, prediction_data = prediction_data,
                            model_input_options = model_input_options)

    train_out <- reg_out$training

    final_train_matrix <- train_out$training_load_pred

    # Run only if prediction list is available
    if(! is.null(prediction_data)) {
      pred_out <- reg_out$predictions
      final_pred_matrix <- pred_out$pred_vec
    }

    # Run for demand modeling - timescale_days used
  } else {

    modeled_demand <- model_demand_with_TOWT(training_data = training_data, prediction_data = prediction_data, model_input_options = model_input_options)

    final_train_matrix <- modeled_demand$final_train_matrix

    reg_out <- modeled_demand$reg_out

    # Run only if prediction list is available
    if(! is.null(prediction_data)) {
      pred_out <- reg_out$predictions
      final_pred_matrix <- modeled_demand$final_pred_matrix
    }

  }

  results <- list()

  # training data and model fit
  results$training_data <- dplyr::bind_cols(training_data, "model_fit" = final_train_matrix)

  # Run only if prediction list is available
  if(! is.null(prediction_data)){
    results$prediction_data <- dplyr::bind_cols(prediction_data, "model_predictions" = final_pred_matrix)
  }

  # model fits and coefficient counts for models
  if(model_input_options$regression_type == "TOW") {
    results$model_occupied <- reg_out$model
  } else {
      results$model_occupied <- reg_out$model_occupied
      results$model_unoccupied <- reg_out$model_unoccupied
  }

  results$model_input_options <- model_input_options

  return(results)

}
