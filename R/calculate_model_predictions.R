

calculate_model_predictions <- function(prediction_list = NULL, modeled_object = NULL, model_input_options = NULL) {

  if(model_input_options$regression_type == "SLR") {

    dframe_pred <- prediction_list$dataframe

    if(! is.null(prediction_list$operating_mode_data)){
      dframe_pred <- dplyr::inner_join(dframe_pred, prediction_list$operating_mode_data, by = "time")
    }

    dframe_pred <- dframe_pred %>%
      dplyr::select(-"time")

  } else if (model_input_options$regression_type == "Three Parameter Cooling" | model_input_options$regression_type == "Three Parameter Heating" |
             model_input_options$regression_type == "Four Parameter Linear Model" | model_input_options$regression_type == "Five Parameter Linear Model") {

    dframe_pred <- prediction_list$dataframe$temp

  }

  predictions <- predict(modeled_object$model, newdata = dframe_pred)

  out <- NULL
  out$predictions <- data.frame(prediction_list$dataframe, predictions)

}
