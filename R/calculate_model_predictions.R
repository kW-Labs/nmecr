#' Calculate model predictions
#'
#' \code{This function calculates predictions from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a list with the following components:
#' \describe{
#'   \item{predictions} {dataframe with model predictions}
#' }
#'
#' @export

calculate_model_predictions <- function(training_data = NULL, prediction_data = NULL, modeled_object = NULL) {

  dframe_pred <- prediction_data

  if(modeled_object$model_input_options$regression_type == "SLR" |
     modeled_object$model_input_options$regression_type == "HDD-CDD Multivariate Regression" |
     modeled_object$model_input_options$regression_type == "HDD Regression" |
     modeled_object$model_input_options$regression_type == "CDD Regression") {

    predictions <- predict(modeled_object$model, dframe_pred)

    predictions_df <- data.frame(prediction_data, predictions)

  } else if(modeled_object$model_input_options$regression_type == "TOWT" |
            modeled_object$model_input_options$regression_type == "TOW") {

    predictions <- calculate_TOWT_model_predictions(training_data = training_data, prediction_data = prediction_data,
                                                    modeled_object = modeled_object)

    predictions_df <- data.frame(prediction_data, predictions)

  } else if (modeled_object$model_input_options$regression_type == "Three Parameter Cooling" | modeled_object$model_input_options$regression_type == "Three Parameter Heating" |
             modeled_object$model_input_options$regression_type == "Four Parameter Linear Model" | modeled_object$model_input_options$regression_type == "Five Parameter Linear Model") {

    dframe_pred <- data.frame(independent_variable = prediction_data$temp)

    predictions <- segmented::predict.segmented(object = modeled_object$model, newdata = dframe_pred)

    predictions_df <- data.frame(prediction_data, predictions)

  }

  return(predictions_df)

}
