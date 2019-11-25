#' Calculate TOWT model predictions
#'
#' \code{This function calculates predictions from model_with_TOWT}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#' @return a list with the following components:
#' \describe{
#'   \item{predictions} {dataframe with model predictions}
#' }
#'

calculate_TOWT_model_predictions <- function(training_data = NULL, prediction_data = NULL, modeled_object = NULL){

  # Create training data temperature matrix
  temp_mat <- create_temp_matrix(training_data$temp, modeled_object$model_input_options$calculated_temp_knots)
  temp_m_name <- rep(NA, ncol(temp_mat))
  for (i in 1 : ncol(temp_mat)) {
    temp_m_name[i] <- paste("temp_mat", i, sep = "")
  }
  names(temp_mat) <- temp_m_name

  # Create prediction data temperature matrix ----
  temp_mat_pred <- create_temp_matrix(prediction_data$temp, modeled_object$model_input_options$calculated_temp_knots)
  names(temp_mat_pred) <- temp_m_name

  # Create prediction dataframe based on interval of week ----
  minute_of_week_pred <- (lubridate::wday(prediction_data$time) - 1) * 24 * 60 +
    lubridate::hour(prediction_data$time) * 60 + lubridate::minute(prediction_data$time)

  interval_of_week_pred <- 1 + floor(minute_of_week_pred / modeled_object$model_input_options$interval_minutes)

  ftow <- factor(interval_of_week_pred)

  dframe_pred <- data.frame(prediction_data, ftow)

  if(modeled_object$model_input_options$chosen_modeling_interval == "Hourly") {
    dframe_pred <- dframe_pred %>%
      select(-c("time", "temp"))
  } else if (modeled_object$model_input_options$chosen_modeling_interval == "Daily") {
    dframe_pred <- dframe_pred %>%
      select(-c("time", "temp", "HDD", "CDD"))
  }

  # Time-of-Week ----

  if (modeled_object$model_input_options$regression_type == "TOW") {

    ok_tow_pred <- factor(ftow) %in% modeled_object$model_occupied$xlevels$ftow
    predictions <- rep(NA, length(prediction_data$time))
    predictions[ok_tow_pred] <- predict(modeled_object$model_occupied, dframe_pred)

  } else {

    # Determine occupancy information

    ok_load <- !is.na(training_data$eload)

    minute_of_week <- (lubridate::wday(training_data$time) - 1) * 24 * 60 +
      lubridate::hour(training_data$time) * 60 + lubridate::minute(training_data$time)

    interval_of_week <- 1 + floor(minute_of_week / modeled_object$model_input_options$interval_minutes)

    occ_info <- find_occ_unocc(interval_of_week[ok_load],
                               training_data$eload[ok_load], training_data$temp[ok_load])
    occ_intervals <- occ_info[occ_info[, 2] == 1, 1]

    occ_vec <- rep(0, length(training_data$eload))

    if (length(occ_intervals) > 2) {
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }
    }


    # Add temperature matrix information to the prediction dataframe
    dframe_pred <- data.frame(dframe_pred, temp_mat_pred)

    predictions <- rep(NA, length(prediction_data$time))

    # create subsets by occupancy - predict for each subset
    ok_occ <- occ_vec == 1
    ok_occ[is.na(ok_occ)] <- TRUE

    if (sum(ok_occ > 0)) {
      ok_tow_pred <- dframe_pred$ftow %in% modeled_object$model_occupied$xlevels$ftow
      predictions[ok_tow_pred] <- predict(modeled_object$model_occupied, dframe_pred[ok_tow_pred, ])
    }

    if (sum(! ok_occ) > 0) {
      ok_tow_pred <- dframe_pred$ftow %in% modeled_object$model_unoccupied$xlevels$ftow
      predictions[ok_tow_pred] <- predict(modeled_object$model_unoccupied, dframe_pred[ok_tow_pred, ])
    }

  }

  output <- NULL
  predictions[predictions < 0] <- 0
  output <-  predictions

  return(output)

}
