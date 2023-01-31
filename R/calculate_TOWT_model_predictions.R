#' Calculate TOWT model predictions
#'
#' \code{This function calculates predictions from model_with_TOWT}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param modeled_object  List with model results. Output from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT.
#' @param allow_neg_predict Boolean determining whether negative predictions are allowed for TOWT models.
#'
#' @importFrom magrittr %>%
#'
#' @return a list with the following components:
#' \describe{
#'   \item{predictions}{dataframe with model predictions}
#' }
#'

calculate_TOWT_model_predictions <- function(training_data = NULL, prediction_data = NULL, modeled_object = NULL, allow_neg_predict = FALSE){

  if(! is.null(modeled_object$model_input_options$timecale_days)) {
    stop("Error: Cannot make predictions with a weighted model yet. This functionality will be available in future nmecr releases. Use model_with_TOWT() for predictions.")
  } else {

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

    # prepare interval of week
    minute_of_week <- (lubridate::wday(training_data$time) - 1) * 24 * 60 +
      lubridate::hour(training_data$time) * 60 + lubridate::minute(training_data$time)

    interval_of_week <- 1 + floor(minute_of_week / modeled_object$model_input_options$interval_minutes)

    ftow <- factor(interval_of_week)
    dframe <- data.frame(training_data, ftow)

    # Create prediction dataframe based on interval of week ----
    minute_of_week_pred <- (lubridate::wday(prediction_data$time) - 1) * 24 * 60 +
      lubridate::hour(prediction_data$time) * 60 + lubridate::minute(prediction_data$time)

    interval_of_week_pred <- 1 + floor(minute_of_week_pred / modeled_object$model_input_options$interval_minutes)

    ftow <- factor(interval_of_week_pred)
    dframe_pred <- data.frame(prediction_data, ftow)

    if(modeled_object$model_input_options$chosen_modeling_interval == "Hourly") {
      dframe_pred <- dframe_pred %>%
        dplyr::select(-c("time", "temp"))
    } else if (modeled_object$model_input_options$chosen_modeling_interval == "Daily") {
      dframe_pred <- dframe_pred %>%
        dplyr::select(-c("time", "temp", "HDD", "CDD"))
    }

    # Time-of-Week ----

    if (modeled_object$model_input_options$regression_type == "TOW") {

      ok_tow_pred <- factor(ftow) %in% modeled_object$model_occupied$xlevels$ftow
      predictions <- rep(NA, nrow(prediction_data))

      id <- which(!(dframe_pred$ftow %in% levels(dframe$ftow))) # remove extra levels before calculation
      dframe_pred$ftow[id] <- NA
      predictions[ok_tow_pred] <- stats::predict(modeled_object$model_occupied, dframe_pred)

    } else {

      # Determine occupancy information

      occ_info <- modeled_object$model_input_options$occupancy_info
      occ_intervals <- occ_info[occ_info[, 2] == 1, 1]


      # create an occupancy vector for training dataset
      occ_vec <- rep(0, nrow(training_data))
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }


      #create an occupancy vector for prediction dataframe
      occ_vec_pred <- rep(0, nrow(prediction_data))
      for (i in 1 : length(occ_intervals)) {
        occ_vec_pred[interval_of_week_pred == occ_intervals[i]] <- 1
      }

      # Add temperature matrix information to the prediction dataframe
      dframe_pred <- data.frame(dframe_pred, temp_mat_pred)

      predictions <- rep(NA, nrow(prediction_data))

      # create subsets by occupancy - predict for each subset
      ok_occ <- occ_vec == 1
      ok_occ[is.na(ok_occ)] <- TRUE

      ok_occ_pred <- occ_vec_pred == 1
      ok_occ_pred[is.na(ok_occ_pred)] <- TRUE

      if (sum(ok_occ > 0)) {

        if(nlevels(factor(dframe[ok_occ,]$ftow)) == 1) { # drop ftow if only one level is present
          dframe_occ <- dframe %>%
            dplyr::select(-"ftow")
        } else {
          dframe_occ <- dframe
        }

        if("ftow" %in% colnames(dframe_occ)){
          id <- which(!(dframe_pred$ftow %in% levels(dframe_occ$ftow))) # remove extra levels before calculation
          dframe_pred$ftow[id] <- NA
        }
        predictions[ok_occ_pred] <- stats::predict(modeled_object$model_occupied, dframe_pred[ok_occ_pred, ])
      }

      if (sum(! ok_occ) > 0) {

        if(nlevels(factor(dframe[! ok_occ,]$ftow)) == 1) { # drop ftow if only one level is present
          dframe_unocc <- dframe %>%
            dplyr::select(-"ftow")
        } else {
          dframe_unocc <- dframe
        }

        if("ftow" %in% colnames(dframe_unocc)){
          id <- which(!(dframe_pred$ftow %in% levels(dframe_unocc$ftow))) # remove extra levels before calculation
          dframe_pred$ftow[id] <- NA
        }
        predictions[! ok_occ_pred] <- stats::predict(modeled_object$model_unoccupied, dframe_pred[! ok_occ_pred, ])
      }

    }

    output <- NULL
    if (allow_neg_predict == FALSE) {
      predictions[predictions < 0] <- 0
    }
    output <-  predictions

    return(output)
  }

}
