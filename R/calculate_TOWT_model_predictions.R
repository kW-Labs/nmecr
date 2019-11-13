
calculate_TOWT_model_predictions <- function(training_list = NULL, prediction_list = NULL, modeled_object = NULL, model_input_options = NULL){

  # calculate temperature knots ----
  model_input_options$calculated_temp_knots <- calculate_temp_knots(training_list = training_list, model_input_options = model_input_options)

  # Create training data temperature matrix
  temp_mat <- create_temp_matrix(training_list$dataframe$temp, model_input_options$calculated_temp_knots)
  temp_m_name <- rep(NA, ncol(temp_mat))
  for (i in 1 : ncol(temp_mat)) {
    temp_m_name[i] <- paste("temp_mat", i, sep = "")
  }
  names(temp_mat) <- temp_m_name

  # Create prediction data temperature matrix ----
  temp_mat_pred <- create_temp_matrix(prediction_list$dataframe$temp, model_input_options$calculated_temp_knots)
  names(temp_mat_pred) <- temp_m_name

  # Create prediction dataframe based on interval of week ----
  minute_of_week_pred <- (lubridate::wday(prediction_list$dataframe$time) - 1) * 24 * 60 +
    lubridate::hour(prediction_list$dataframe$time) * 60 + lubridate::minute(prediction_list$dataframe$time)

  interval_of_week_pred <- 1 + floor(minute_of_week_pred / model_input_options$interval_minutes)

  ftow <- factor(interval_of_week_pred)

  dframe_pred <- data.frame("time" = prediction_list$dataframe$time, ftow)

  if (! is.null(prediction_list$operating_mode_data)) {
    dframe_pred <- dplyr::inner_join(dframe_pred, prediction_list$operating_mode_data, by = "time")
  }

  # Time-only ----

  if (model_input_options$regression_type == "Time-only") {

    ok_tow_pred <- factor(ftow) %in% modeled_object$model_occupied$xlevels$ftow
    pred_vec <- rep(NA, length(prediction_list$dataframe$time))
    pred_vec[ok_tow_pred] <- predict(modeled_object$model_occupied, dframe_pred)

  } else {

    # Determine occupancy information

    ok_load <- !is.na(training_list$dataframe$eload)

    minute_of_week <- (lubridate::wday(training_list$dataframe$time) - 1) * 24 * 60 +
      lubridate::hour(training_list$dataframe$time) * 60 + lubridate::minute(training_list$dataframe$time)

    interval_of_week <- 1 + floor(minute_of_week / model_input_options$interval_minutes)

    occ_info <- find_occ_unocc(interval_of_week[ok_load],
                               training_list$dataframe$eload[ok_load], training_list$dataframe$temp[ok_load])
    occ_intervals <- occ_info[occ_info[, 2] == 1, 1]

    occ_vec <- rep(0, length(training_list$dataframe$eload))

    if (length(occ_intervals) > 2) {
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }
    }


    # Add temperature matrix information to the prediction dataframe
    dframe_pred <- data.frame(dframe_pred, temp_mat_pred)

    pred_vec <- rep(NA, length(prediction_list$dataframe$time))

    # create subsets by occupancy - predict for each subset
    ok_occ <- occ_vec == 1
    ok_occ[is.na(ok_occ)] <- TRUE

    if (sum(ok_occ > 0)) {
      ok_tow_pred <- dframe_pred$ftow %in% modeled_object$model_occupied$xlevels$ftow
      pred_vec[ok_tow_pred] <- predict(modeled_object$model_occupied, dframe_pred[ok_tow_pred, ])
    }

    if (sum(! ok_occ) > 0) {
      ok_tow_pred <- dframe_pred$ftow %in% modeled_object$model_unoccupied$xlevels$ftow
      pred_vec[ok_tow_pred] <- predict(modeled_object$model_unoccupied, dframe_pred[ok_tow_pred, ])
    }

  }

  output <- NULL
  pred_vec[pred_vec < 0] <- 0
  output <-  pred_vec

  return(output)

}
