#' Modeling electric demand Time-of-Week and Temperature.
#'
#' \code{This function builds an electric demand models using two algorithms: TOWT and MW.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a list with the following components:
#' \describe{
#'   \item{final_train_matrix} {training data matrix weighted according to timescale_days}
#'   \item{reg_out} {an lm object generated from fit_TOWT_reg}
#'   \item{final_pred_matrix} {prediction data matrix weighted according to timescale_days}
#' }
#'
#' @export

model_demand_with_TOWT <- function(training_data = NULL, prediction_data = NULL, model_input_options = NULL){

  num_points <- length(training_data$time)
  t0 <- min(training_data$time, na.rm = TRUE)
  t1 <- max(training_data$time, na.rm = TRUE)

  delta_t <- as.numeric(difftime(t1, t0, units = "days"))
  num_segments <- max(1, ceiling(delta_t / model_input_options$timescale_days))
  segment_width <- (num_points - 1) / num_segments
  point_list <- floor(sort(num_points - segment_width * (0 : num_segments)) +
                        0.001)

  num_model_runs <- max(1, length(point_list))

  # Creating weighting matrices for training data
  train_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(training_data$time))

  train_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(training_data$time))

  results <- list()

  # Run only if prediction list is available
  if(! is.null(prediction_data)) {
    pred_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_data$time))
    pred_weight_matrix <- matrix(NA, nrow = num_model_runs, ncol = length(prediction_data$time))
  }

  # weighting wrapper around fit_towt_reg()
  for (row_index in 1 : num_model_runs) {
    tcenter <- training_data$time[point_list[row_index]]
    t_diff <- as.numeric(difftime(tcenter, training_data$time, units = "days"))

    model_input_options$train_weight_vec <- model_input_options$timescale_days ^ 2 /
      (model_input_options$timescale_days ^ 2 + t_diff ^ 2)

    # fit linear regression
    reg_out <- fit_TOWT_reg(training_data = training_data, prediction_data = prediction_data,
                            model_input_options = model_input_options)

    train_out <- reg_out$training

    train_matrix[row_index, ] <- train_out$training_load_pred
    train_weight_matrix[row_index, ] <- model_input_options$train_weight_vec

    final_train_matrix <- apply(train_matrix * train_weight_matrix, 2, sum) /
      apply(train_weight_matrix, 2, sum)

    results$final_train_matrix <- final_train_matrix
    results$reg_out <- reg_out

    # Run only if prediction list is available
    if(! is.null(prediction_data)) {

      t_diff_pred <- as.numeric(difftime(tcenter, prediction_data$time, units = "days"))

      model_input_options$pred_weight_vec <- model_input_options$timescale_days ^ 2 /
        (model_input_options$timescale_days ^ 2 + t_diff_pred ^ 2)

      pred_out <- reg_out$predictions

      pred_matrix[row_index, ] <- pred_out$pred_vec
      pred_weight_matrix[row_index, ] <- model_input_options$pred_weight_vec

      final_pred_matrix <- apply(pred_matrix * pred_weight_matrix, 2, sum) /
        apply(pred_weight_matrix, 2, sum)

      results$final_pred_matrix <- final_pred_matrix
    }

  }

  return(results)

}
