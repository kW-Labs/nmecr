


create_weighted_predictions <- function(model_object = NULL, training_list = NULL, prediction_list = NULL, model_input_options = NULL) {
  
  model_input_options$calculated_temp_knots <- calculate_temp_knots(training_list = training_list, model_input_options = model_input_options)
  
  if (model_input_options$timescale_days == "NA") {
    
    predictions <- create_model_predictions(model_object = model_object, training_list = training_list, prediction_list = prediction_list,
                                            model_input_options = model_input_options)
    
    final_pred_matrix <- predictions$pred_vec
    
  } else {
    
    num_points <- length(training_list$dataframe$time)
    t0 <- min(training_list$dataframe$time, na.rm = TRUE)
    t1 <- max(training_list$dataframe$time, na.rm = TRUE)
    
    delta_t <- as.numeric(difftime(t1, t0, units = "days"))
    num_segments <- max(1, ceiling(delta_t / model_input_options$timescale_days))
    segment_width <- (num_points - 1) / num_segments
    point_list <- floor(sort(num_points - segment_width * (0 : num_segments)) +
                          0.001)
    
    num_model_runs <- max(1, length(point_list))
    
    pred_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_list$dataframe$time))
    pred_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_list$dataframe$time))
    
    for (row_index in 1 : num_model_runs) {
      
      predictions <- create_model_predictions(model_object = model_object, training_list = training_list, prediction_list = prediction_list,
                                              model_input_options = model_input_options)
      
      tcenter <- training_list$dataframe$time[point_list[row_index]]
      t_diff_pred <- as.numeric(difftime(tcenter, prediction_list$dataframe$time, units = "days"))
      
      model_input_options$pred_weight_vec <- model_input_options$timescale_days ^ 2 /
        (model_input_options$timescale_days ^ 2 + t_diff_pred ^ 2)
      
      pred_matrix[row_index, ] <- predictions$pred_vec
      pred_weight_matrix[row_index, ] <- model_input_options$pred_weight_vec
      
      final_pred_matrix <- apply(pred_matrix * pred_weight_matrix, 2, sum) /
        apply(pred_weight_matrix, 2, sum)
    }
    
  }
  
  predictions <-  dplyr::bind_cols(prediction_list$dataframe, "model_predictions" = final_pred_matrix)
  
  return(predictions)
    

}