


model_demand_with_TOWT <- function(training_list = NULL, prediction_list = NULL, model_input_options = NULL){
  
  num_points <- length(training_list$dataframe$time)
  t0 <- min(training_list$dataframe$time, na.rm = TRUE)
  t1 <- max(training_list$dataframe$time, na.rm = TRUE)
  
  delta_t <- as.numeric(difftime(t1, t0, units = "days"))
  num_segments <- max(1, ceiling(delta_t / model_input_options$timescale_days))
  segment_width <- (num_points - 1) / num_segments
  point_list <- floor(sort(num_points - segment_width * (0 : num_segments)) +
                        0.001)
  
  num_model_runs <- max(1, length(point_list))
  
  # Creating weighting matrices for training data
  train_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(training_list$dataframe$time))
  
  train_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(training_list$dataframe$time))
  
  results <- list()
  results$final_train_matrix <- final_train_matrix
  
  # Run only if prediction list is available
  if(! is.null(prediction_list)) {
    pred_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_list$dataframe$time))
    pred_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_list$dataframe$time))
  }
  
  # weighting wrapper around fit_towt_reg()
  for (row_index in 1 : num_model_runs) {
    tcenter <- training_list$dataframe$time[point_list[row_index]]
    t_diff <- as.numeric(difftime(tcenter, training_list$dataframe$time, units = "days"))
    
    model_input_options$train_weight_vec <- model_input_options$timescale_days ^ 2 /
      (model_input_options$timescale_days ^ 2 + t_diff ^ 2)
    
    # fit linear regression
    reg_out <- fit_TOWT_reg(training_list = training_list, prediction_list = prediction_list,
                            model_input_options = model_input_options)
    
    train_out <- reg_out$training
    
    train_matrix[row_index, ] <- train_out$training_load_pred
    train_weight_matrix[row_index, ] <- model_input_options$train_weight_vec
    
    final_train_matrix <- apply(train_matrix * train_weight_matrix, 2, sum) /
      apply(train_weight_matrix, 2, sum)
    
    # Run only if prediction list is available
    if(! is.null(prediction_list)) {
      
      t_diff_pred <- as.numeric(difftime(tcenter, prediction_list$dataframe$time, units = "days"))
      
      model_input_options$pred_weight_vec <- model_input_options$timescale_days ^ 2 /
        (model_input_options$timescale_days ^ 2 + t_diff_pred ^ 2)
      
      pred_out <- reg_out$predictions
      
      pred_matrix[row_index, ] <- pred_out$pred_vec
      pred_weight_matrix[row_index, ] <- pred_weight_vec
      
      final_pred_matrix <- apply(pred_matrix * pred_weight_matrix, 2, sum) /
        apply(pred_weight_matrix, 2, sum)
      
      results$final_pred_matrix <- final_pred_matrix
    }
    
  }
  

 
  
  return(results)
  
}
