

calculate_model_predictions <- function(prediction_list = NULL, modeled_object = NULL, model_input_options = NULL) {

  dframe_pred <- prediction_list$dataframe

  if(model_input_options$regression_type == "TOWT" |
     model_input_options$regression_type == "Time-only" |
     model_input_options$regression_type == "SLR" |
     model_input_options$regression_type == "HDD-CDD Multivariate Regression" |
     model_input_options$regression_type == "HDD Regression" |
     model_input_options$regression_type == "CDD Regression") {

    if(! is.null(prediction_list$operating_mode_data)){
      dframe_pred <- dplyr::inner_join(dframe_pred, prediction_list$operating_mode_data, by = "time")
    }

    # if column names are different, throw an error

    predictions <- predict(modeled_object$model, dframe_pred)

    out <- NULL
    out$predictions <- data.frame(prediction_list$dataframe, predictions)

  } else if (model_input_options$regression_type == "Three Parameter Cooling" | model_input_options$regression_type == "Three Parameter Heating" |
             model_input_options$regression_type == "Four Parameter Linear Model" | model_input_options$regression_type == "Five Parameter Linear Model") {

    dframe_pred <- data.frame(independent_variable = prediction_list$dataframe$temp)

    predictions <- segmented::predict.segmented(object = modeled_object$model, newdata = dframe_pred)

    out <- NULL
    out$predictions <- data.frame(prediction_list$dataframe, predictions)

  }

  return(out)

}

