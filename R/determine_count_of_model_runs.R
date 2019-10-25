#' Determine number of model runs
#'
#' \code{This function determines the number of model runs to optimize for energy demand predictions.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param training_data Training period dataframe, an object that has been created by the function create_dataframe() and add_operating_mode_data()
#' @param timescale_days Numeric correspond to the timescale for weighting function.Default: NULL.
#' Change to improve accuracy of short term models. 
#'
#' @return a numeric corresponding to the number of model runs to be computed for TOWT
#'
#' @export
#' 


created_weighted_dataframes <- function(training_data = NULL, prediction_data, timescale_days = NULL) {
  
  if(! is.null(timescale_days) | ! assertive::is_numeric(timescale_days)) {
    
    stop("timescale_days is a non-numeric. Please provide a number or leave as NULL")
    
  } else { 
    
    num_points <- length(training_data$time)
    t0 <- min(training_data$time, na.rm = TRUE)
    t1 <- max(training_data$time, na.rm = TRUE)
    
    delta_t <- as.numeric(difftime(t1, t0, units = "days"))
    num_segments <- max(1, ceiling(delta_t / timescale_days))
    segment_width <- (num_points - 1) / num_segments
    point_list <- floor(sort(num_points - segment_width * (0 : num_segments)) +
                          0.001)
    
    train_matrix <- matrix(NA, nrow = num_model_runs,
                           ncol = length(training_data$time))
    
    train_weight_matrix <- matrix(NA, nrow = num_model_runs,
                                  ncol = length(training_data$time))
    
    pred_matrix <- matrix(NA, nrow = num_model_runs,
                          ncol = length(prediction_data$time))
    
    pred_weight_matrix <- matrix(NA, nrow = num_model_runs,
                                 ncol = length(prediction_data$time)
                                 
                                 
    for (row_index in 1 : num_model_runs) {
      tcenter <- time_col[point_list[row_index]]
      t_diff <- as.numeric(difftime(tcenter, training_data$time, units = "days"))
      t_diff_pred <- as.numeric(difftime(tcenter, prediction_data$time, units = "days"))
    }
    
    # determine number of model runs and create weighted matrices
                                   
      if (is.null(timescale_days)) {
    
        num_model_runs <- 1
    
        train_weight_vec <- rep(1, length(training_data$time))
    
        pred_weight_vec <- rep(1, length(prediction_data$time)
    
    
      } else if (assertive::is_numeric(timescale_days) {
    
        num_model_runs <- max(1, length(point_list))
    
        train_weight_vec <- timescale_days ^ 2 /
          (timescale_days ^ 2 + t_diff ^ 2)
    
        pred_weight_vec <- timescale_days ^ 2 /
          (timescale_days ^ 2 + t_diff_pred ^ 2)
      }
    
    results <- list()
    
    resutlts$num_model_runs <- num_model_runs
    results$train_weight_vec <- train_weight_vec
    results$pred_weight_vec <- pred_weight_vec
    
  }
  
  return(results)
  
}

