#' Calculate model predictions
#'
#' \code{This function calculates predictions from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param modeled_object List with model results. Output from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT.
#'
#' @return a list with the following components:
#' \describe{
#'   \item{predictions}{dataframe with model predictions}
#' }
#'
#' @export

calculate_model_predictions <- function(training_data = NULL, prediction_data = NULL, modeled_object = NULL) {

  training_data <- training_data[complete.cases(training_data), ] # remove any incomplete observations

  prediction_data <- prediction_data[complete.cases(prediction_data), ] # remove any incomplete observations

  if (modeled_object$model_input_options$regression_type == "SLR" |
     modeled_object$model_input_options$regression_type == "HDD Regression" | modeled_object$model_input_options$regression_type == "HDD" |
     modeled_object$model_input_options$regression_type == "CDD Regression" | modeled_object$model_input_options$regression_type == "CDD" |
     modeled_object$model_input_options$regression_type == "HDD-CDD Multivariate Regression" | modeled_object$model_input_options$regression_type == "HDD-CDD") {

    if(modeled_object$model_input_options$day_normalized) { # Day-Normalized

      predictions <- predict(modeled_object$model, prediction_data) %>%
        magrittr::multiply_by(prediction_data$days)

      predictions_df <- data.frame(prediction_data, predictions)

    } else { # Hourly or Daily

      predictions <- predict(modeled_object$model, prediction_data)

      predictions_df <- data.frame(prediction_data, predictions)

    }

  } else if (modeled_object$model_input_options$regression_type == "TOWT" |
            modeled_object$model_input_options$regression_type == "TOW") { # Hourly or Daily only

    predictions <- calculate_TOWT_model_predictions(training_data = training_data, prediction_data = prediction_data,
                                                    modeled_object = modeled_object)

    predictions_df <- data.frame(prediction_data, predictions)

  } else {

    if (modeled_object$model_input_options$regression_type == "Three Parameter Cooling" | modeled_object$model_input_options$regression_type == "3PC" |
        modeled_object$model_input_options$regression_type == "Four Parameter Linear Model" | modeled_object$model_input_options$regression_type == "4P" |
        modeled_object$model_input_options$regression_type == "Five Parameter Linear Model" | modeled_object$model_input_options$regression_type == "5P") {

      dframe_pred <- data.frame(independent_variable = prediction_data$temp)

    } else if (modeled_object$model_input_options$regression_type == "Three Parameter Heating" | modeled_object$model_input_options$regression_type == "3PH"){

      dframe_pred <- data.frame(independent_variable = - prediction_data$temp)

    }

    if(modeled_object$model_input_options$day_normalized) { # Day-Normalized

      predictions <- segmented::predict.segmented(object = modeled_object$model, newdata = dframe_pred) %>%
        magrittr::multiply_by(prediction_data$days)

      predictions_df <- data.frame(prediction_data, predictions)

    } else { # Hourly or Daily

      predictions <- segmented::predict.segmented(object = modeled_object$model, newdata = dframe_pred)

      predictions_df <- data.frame(prediction_data, predictions)

    }
  }

  return(predictions_df)

}
