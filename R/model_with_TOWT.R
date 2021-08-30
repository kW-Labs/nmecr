#' Time-of-Week and Temperature.
#'
#' \code{This function builds an energy use/demand models using two algorithms: TOWT and MW.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param training_data Training dataframe.Output from create_dataframe function
#' @param prediction_data Prediction dataframe. Output from create_dataframe function
#' @param model_input_options List with model inputs specified using assign_model_inputs function
#' @param occupancy_info An nx2 dataframe with the occupancy information of the training dataset. Column names should be: uniq_time_of_week and ok_occ
#' uniq_time_of_week is the unique time of week (1 through 7 for daily data and 1 through 168 for hourly data).
#' ok_occ is a binary: 1 for occupied and 0 for unoccupied
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model_occupied}{an lm object generated when using Time-of-Week and TIme-of-Week & Temperature algorithms}
#'   \item{model_unoccupied}{an lm object generated when using TOWT algorithm when the unoccupied period is discernably different from the occupied period}
#'   \item{training_data}{training dataframe along with the model_fit values}
#'   \item{prediction_data}{prediction dataframe along with the model prediction values. Only generated when prediction_list is supplied to the algorithm}
#'   \item{model_input_options}{model_input_options from the input along with additional model specifications.}
#' }
#' @note training_data and model_input_options are required arguments for the function.
#' @export

model_with_TOWT <- function(training_data = NULL, prediction_data = NULL, model_input_options = NULL, occupancy_info = NULL){

  training_data <- training_data[stats::complete.cases(training_data), ] # remove any incomplete observations

  if(! is.null(prediction_data)) {
    prediction_data <- prediction_data[stats::complete.cases(prediction_data), ]
  } # remove any incomplete observations

  nterval <-  stats::median(diff(as.numeric(training_data$time)))/60

  #calculate interval minutes
  model_input_options$interval_minutes <- nterval

  if (nterval == 15){
    nterval_value <- "15-min"
  } else if (nterval == 60){
    nterval_value <- "Hourly"
  } else if (nterval == 1440) {
    nterval_value <- "Daily"
  } else if (nterval >= 40320) {
    nterval_value <- "Monthly"
  }

  model_input_options$chosen_modeling_interval <- nterval_value

  if(model_input_options$chosen_modeling_interval == "Monthly") {
    stop("Error: model_with_TOWT cannot be used with Monthly data.")
  }


  if(is.null(occupancy_info)) {
    occupancy_info <- find_occ_unocc(training_data = training_data, model_input_options = model_input_options)
  } else {

    #checks for uploaded occupancy information

    if(model_input_options$chosen_model == "Hourly" & nrow(occupancy_info) != 168) {
      stop("Error: Please make sure the occupancy_info dataframe has 168 rows corresponding to 168 unique times of week")
    } else if (model_input_options$chosen_model == "Daily" & nrow(occupancy_info) != 7){
      stop("Error: Please make sure the occupancy_info dataframe has 7 rows corresponding to 7 unique times of week")
    }

    if(!assertive::is_data.frame(occupancy_info)){
      stop("Error: Please make sure the uploaded occupancy infomation is a dataframe.")
    }

    if(colnames(occupancy_info)[1] != "uniq_time_of_week" | colnames(occupancy_info)[2] != "ok_occ"){
      stop("Error: Please make sure the uploaded occupancy information is a dataframe with column names: 'uniq_time_of_week' and 'ok_occ'.")
    }
  }

  # add_occupancy_info to model_input_options

  model_input_options$occupancy_info <- occupancy_info

  # calculate temperature knots (only for TOWT)
  if (model_input_options$regression_type == 'TOWT') {
    model_input_options$calculated_temp_knots <- calculate_temp_knots(training_data = training_data, model_input_options = model_input_options)
  }


  # Run for energy modeling - timescale_days not used
  if (is.null(model_input_options$timescale_days)) {

    # train_weight_vec not essentially needed for energy modeling.
    # it is needed for fit_TOWT_reg however
    # therefore, keeping it as 1 for energy modeling
    model_input_options$train_weight_vec <- rep(1, length(training_data$time))

    # fit linear regression
    reg_out <- fit_TOWT_reg(training_data = training_data, prediction_data = prediction_data,
                            model_input_options = model_input_options)

    train_out <- reg_out$training

    final_train_matrix <- train_out$training_load_pred

    # Run only if prediction list is available
    if(! is.null(prediction_data)) {
      pred_out <- reg_out$predictions
      final_pred_matrix <- pred_out$pred_vec
    }

    # Run for demand modeling - timescale_days used
  } else {

    modeled_demand <- model_demand_with_TOWT(training_data = training_data, prediction_data = prediction_data, model_input_options = model_input_options)

    final_train_matrix <- modeled_demand$final_train_matrix

    reg_out <- modeled_demand$reg_out

    # Run only if prediction list is available
    if(! is.null(prediction_data)) {
      pred_out <- reg_out$predictions
      final_pred_matrix <- modeled_demand$final_pred_matrix
    }

  }

  results <- list()

  # training data and model fit
  results$training_data <- dplyr::bind_cols(training_data, "model_fit" = final_train_matrix)

  # Run only if prediction list is available
  if(! is.null(prediction_data)){
    results$prediction_data <- dplyr::bind_cols(prediction_data, "model_predictions" = final_pred_matrix)
  }

  # model fits and coefficient counts for models
  if(model_input_options$regression_type == "TOW") {
    results$model_occupied <- reg_out$model
    results$model_occupied_stats <- dplyr::bind_cols("Variable" = rownames(summary(reg_out$model)$coeff), as.data.frame(summary(reg_out$model)$coeff))
  } else {
      results$model_occupied <- reg_out$model_occupied
      results$model_unoccupied <- reg_out$model_unoccupied
      if(exists("model_occupied", where = reg_out)) {
        results$model_occupied_stats <- dplyr::bind_cols("Variable" = rownames(summary(reg_out$model_occupied)$coeff), as.data.frame(summary(reg_out$model_occupied)$coeff))
      }

      if (exists("model_unoccupied", where = reg_out)) {
        results$model_unoccupied_stats <- dplyr::bind_cols("Variable" = rownames(summary(reg_out$model_unoccupied)$coeff), as.data.frame(summary(reg_out$model_unoccupied)$coeff))
      }
  }

  results$model_input_options <- model_input_options

  return(results)

}
