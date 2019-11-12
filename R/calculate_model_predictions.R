

calculate_model_predictions <- function(prediction_list = NULL, modeled_object = NULL) {
  
  dframe_pred <- prediction_list$dataframe
  
  if(! is.null(prediction_list$operating_mode_data)){
    dframe_pred <- dplyr::inner_join(dframe_pred, prediction_list$operating_mode_data, by = "time")
  }
  
  dframe_pred <- dframe_pred %>%
    dplyr::select(-"time")
  
  predictions <- predict(modeled_object$model, newdata = dframe_pred)
  
  out <- NULL
  out$predictions <- data.frame(prediction_list$dataframe, predictions)
  
}
