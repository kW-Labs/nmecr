#' Calculate model predictions
#'
#' \code{This function calculates predictions from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param modeled_object List with model results. Output from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT.
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with model predictions
#'
#' @export

calculate_model_predictions <- function(training_data = NULL, prediction_data = NULL, modeled_object = NULL, allow_neg_predict = FALSE) {

  independent_variable <- NULL # No visible binding for global variable

  training_data <- training_data[stats::complete.cases(training_data), ] # remove any incomplete observations

  prediction_data <- prediction_data[stats::complete.cases(prediction_data), ] # remove any incomplete observations

  if (modeled_object$model_input_options$regression_type == "SLR" |
     modeled_object$model_input_options$regression_type == "HDD Regression" | modeled_object$model_input_options$regression_type == "HDD" |
     modeled_object$model_input_options$regression_type == "CDD Regression" | modeled_object$model_input_options$regression_type == "CDD" |
     modeled_object$model_input_options$regression_type == "HDD-CDD Multivariate Regression" | modeled_object$model_input_options$regression_type == "HDD-CDD") {

    if(modeled_object$model_input_options$day_normalized & modeled_object$model_input_options$chosen_modeling_interval == "Monthly") { # Day-Normalized

      predictions <- stats::predict(modeled_object$model, prediction_data) %>%
        magrittr::multiply_by(prediction_data$days)

      predictions_df <- data.frame(prediction_data, predictions)

    } else { # Hourly or Daily

      predictions <- stats::predict(modeled_object$model, prediction_data)

      predictions_df <- data.frame(prediction_data, predictions)

    }

  } else if (modeled_object$model_input_options$regression_type == "TOWT" |
            modeled_object$model_input_options$regression_type == "TOW") { # Hourly or Daily only

    predictions <- calculate_TOWT_model_predictions(training_data = training_data, prediction_data = prediction_data,
                                                    modeled_object = modeled_object, allow_neg_predict = allow_neg_predict)

    predictions_df <- data.frame(prediction_data, predictions)

  } else {

    if (modeled_object$model_input_options$day_normalized == TRUE & modeled_object$model_input_options$chosen_modeling_interval == "Monthly") {
      names(prediction_data)[names(prediction_data) == "eload_perday"] <- "dependent_variable"
    } else {
      names(prediction_data)[names(prediction_data) == "eload"] <- "dependent_variable"
    }

    names(prediction_data)[names(prediction_data) == "temp"] <- "independent_variable"

    if (modeled_object$model_input_options$regression_type == "Three Parameter Cooling" | modeled_object$model_input_options$regression_type == "3PC") {

      dframe_pred <- prediction_data

      breakpoint <- modeled_object$model_input_options$estimated_breakpoint$Est.

      remaining_columns <- names(modeled_object$model$model)[! names(modeled_object$model$model) %in% names(dframe_pred)]

      if("dependent_variable" %in% remaining_columns) { # for normalized predictions
        remaining_columns <- remaining_columns[remaining_columns != "dependent_variable"]
      }

      if (length(remaining_columns) == 1) {
        if (remaining_columns == 'U1.independent_variable') {
          dframe_pred <- dframe_pred %>%
            dplyr::mutate(U1.independent_variable = pmax(independent_variable - breakpoint, 0))
        }
      } else {
        dframe_pred <- dframe_pred
      }

    } else if (modeled_object$model_input_options$regression_type == "Three Parameter Heating" | modeled_object$model_input_options$regression_type == "3PH") {

      dframe_pred <- prediction_data

      breakpoint <- modeled_object$model_input_options$estimated_breakpoint$Est.

      remaining_columns <- names(modeled_object$model$model)[! names(modeled_object$model$model) %in% names(dframe_pred)]

      if("dependent_variable" %in% remaining_columns) { # for normalized predictions
        remaining_columns <- remaining_columns[remaining_columns != "dependent_variable"]
      }

      if (length(remaining_columns) == 1) {
        if (remaining_columns == 'U1.independent_variable') {
          dframe_pred <- dframe_pred %>%
            dplyr::mutate(U1.independent_variable = pmax(breakpoint - independent_variable, 0))
        }
      } else {
        dframe_pred <- dframe_pred
      }

    } else if (modeled_object$model_input_options$regression_type == "Four Parameter Linear Model" | modeled_object$model_input_options$regression_type == "4P") {

      dframe_pred <- prediction_data

      breakpoint <- modeled_object$model_input_options$estimated_breakpoint$Est.

      remaining_columns <- names(modeled_object$model$model)[! names(modeled_object$model$model) %in% names(dframe_pred)]

      if("dependent_variable" %in% remaining_columns) { # for normalized predictions
        remaining_columns <- remaining_columns[remaining_columns != "dependent_variable"]
      }

      if (length(remaining_columns) == 1) {
        if (remaining_columns == 'U1.independent_variable') {
          dframe_pred <- dframe_pred %>%
            dplyr::mutate(U1.independent_variable = pmax(independent_variable - breakpoint, 0))
        }
      } else {
        dframe_pred <- dframe_pred
      }

    } else if (modeled_object$model_input_options$regression_type == "Five Parameter Linear Model" | modeled_object$model_input_options$regression_type == "5P") {

      dframe_pred <- prediction_data

      breakpoint <- modeled_object$model_input_options$estimated_breakpoint$Est.

      remaining_columns <- names(modeled_object$model$model)[! names(modeled_object$model$model) %in% names(dframe_pred)]

      if("dependent_variable" %in% remaining_columns) { # for normalized predictions
        remaining_columns <- remaining_columns[remaining_columns != "dependent_variable"]
      }

      if (length(remaining_columns) == 2) {
        if (remaining_columns[1] == 'U1.independent_variable' &
            remaining_columns[2] == 'U2.independent_variable') {
          dframe_pred <- dframe_pred %>%
            dplyr::mutate(U1.independent_variable = pmax(independent_variable - breakpoint[1], 0),
                        U2.independent_variable = pmax(independent_variable - breakpoint[2], 0))
        }
      } else {
        dframe_pred <- dframe_pred
      }

    }

    if(modeled_object$model_input_options$day_normalized & modeled_object$model_input_options$chosen_modeling_interval == "Monthly") { # Day-Normalized

      predictions <- stats::predict(object = modeled_object$model, newdata = dframe_pred) %>%
        magrittr::multiply_by(prediction_data$days)

      predictions_df <- data.frame(prediction_data, predictions)

      names(predictions_df)[names(predictions_df) == "dependent_variable"] <- "eload_perday"

    } else { # Hourly or Daily

      predictions <- stats::predict(object = modeled_object$model, newdata = dframe_pred)

      predictions_df <- data.frame(prediction_data, predictions)

      names(predictions_df)[names(predictions_df) == "dependent_variable"] <- "eload"

    }
  }

  names(predictions_df)[names(predictions_df) == "independent_variable"] <- "temp"

  return(predictions_df)

}
