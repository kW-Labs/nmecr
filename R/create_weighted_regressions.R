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


create_weighted_regressions <- function(training_list = NULL, prediction_list = NULL,
                                        timescale_days = NULL, interval_minutes = NULL, regression_type = NULL, temp_knots = NULL,
                                        training_operating_mode_data = NULL,
                                        prediction_operating_mode_data = NULL) {

  # Run for energy modeling - timescale_days not used
  if (timescale_days == "NA") {

    num_model_runs <- 1

    train_weight_vec <- rep(1, length(training_list$dataframe$time))

    pred_weight_vec <- rep(1, length(prediction_list$dataframe$time))

    # fit linear regression
    reg_out <- fit_TOWT_reg(training_list = training_list, prediction_list = prediction_list,
                            temp_knots = temp_knots, train_weight_vec = train_weight_vec,
                            interval_minutes = interval_minutes,
                            regression_type = regression_type)

    train_out <- reg_out$training

    pred_out <- reg_out$predictions

    final_train_matrix <- train_out$training_load_pred

    final_pred_matrix <- pred_out$pred_vec

    # Run for demand modeling - timescale_days used
  } else {

    num_points <- length(training_list$dataframe$time)
    t0 <- min(training_list$dataframe$time, na.rm = TRUE)
    t1 <- max(training_list$dataframe$time, na.rm = TRUE)

    delta_t <- as.numeric(difftime(t1, t0, units = "days"))
    num_segments <- max(1, ceiling(delta_t / timescale_days))
    segment_width <- (num_points - 1) / num_segments
    point_list <- floor(sort(num_points - segment_width * (0 : num_segments)) +
                          0.001)

    num_model_runs <- max(1, length(point_list))

    # Creating weighting matrices for training and prediction data
    train_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(training_list$dataframe$time))

    train_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(training_list$dataframe$time))

    pred_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_list$dataframe$time))

    pred_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_list$dataframe$time))

    for (row_index in 1 : num_model_runs) {
      tcenter <- training_list$dataframe$time[point_list[row_index]]
      t_diff <- as.numeric(difftime(tcenter, training_list$dataframe$time, units = "days"))
      t_diff_pred <- as.numeric(difftime(tcenter, prediction_list$dataframe$time, units = "days"))

    train_weight_vec <- timescale_days ^ 2 /
      (timescale_days ^ 2 + t_diff ^ 2)

    pred_weight_vec <- timescale_days ^ 2 /
      (timescale_days ^ 2 + t_diff_pred ^ 2)

    # fit linear regression
    reg_out <- fit_TOWT_reg(training_list = training_list, prediction_list = prediction_list,
                            temp_knots = temp_knots, train_weight_vec = train_weight_vec,
                            interval_minutes = interval_minutes,
                            regression_type = regression_type)

    train_out <- reg_out$training

    pred_out <- reg_out$predictions


    train_matrix[row_index, ] <- train_out$training_load_pred
    train_weight_matrix[row_index, ] <- train_weight_vec

    pred_matrix[row_index, ] <- pred_out$pred_vec
    pred_weight_matrix[row_index, ] <- pred_weight_vec

    final_train_matrix <- apply(train_matrix * train_weight_matrix, 2, sum) /
      apply(train_weight_matrix, 2, sum)

    final_pred_matrix <- apply(pred_matrix * pred_weight_matrix, 2, sum) /
      apply(pred_weight_matrix, 2, sum)

    }
  }


  output <- NULL
  output$final_train_matrix <- final_train_matrix
  output$final_pred_matrix <- final_pred_matrix

  return(output)

}

