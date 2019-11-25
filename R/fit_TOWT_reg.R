#' Utility function for model_with_TOWT
#'
#' \code{This function builds an energy use/demand models separated by occupancy and temperature regimens.}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_data Prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model_occupied} {an lm object generated when using Time-of-Week and Time-of-Week & Temperature algorithms}
#'   \item{model_unoccupied} {an lm object generated when using TOWT algorithm when the unoccupied period is discernably different from the occupied period}
#'   \item{training_data} {training dataframe along with the model_fit values}
#'   \item{prediction_data} {prediction dataframe along with the model prediction values. Only generated when prediction_data is supplied to the algorithm}
#' }

fit_TOWT_reg <- function(training_data = NULL, prediction_data = NULL, model_input_options = NULL) {


  # prepare interval of week for training data
  minute_of_week <- (lubridate::wday(training_data$time) - 1) * 24 * 60 +
    lubridate::hour(training_data$time) * 60 + lubridate::minute(training_data$time)

  interval_of_week <- 1 + floor(minute_of_week / model_input_options$interval_minutes)

  # prepare interval of week for prediction data
  if(! is.null(prediction_data)) {

    minute_of_week_pred <- (lubridate::wday(prediction_data$time) - 1) * 24 * 60 +
      lubridate::hour(prediction_data$time) * 60 + lubridate::minute(prediction_data$time)

    interval_of_week_pred <- 1 + floor(minute_of_week_pred / model_input_options$interval_minutes)
  }

  # run Time-of-Week model

  if (model_input_options$regression_type == "TOW") {

    ftow <- factor(interval_of_week)
    dframe <- data.frame(training_data, ftow)

    if(model_input_options$chosen_modeling_interval == "Hourly") {
      dframe <- dframe %>%
        select(-c("time", "temp"))
    } else if (model_input_options$chosen_modeling_interval == "Daily") {
      dframe <- dframe %>%
        select(-c("time", "temp", "HDD", "CDD"))
    }

    # simple linear regression - no subsetting by occupancy
    amod <- lm(training_data$eload ~ . , data = dframe, na.action = na.exclude, weights = model_input_options$train_weight_vec)
    training_load_pred <- predict(amod)

    # make predictions
    if(! is.null(prediction_data)) {

      ftow <- factor(interval_of_week_pred)
      dframe_pred <- data.frame(prediction_data, ftow)

      if(model_input_options$chosen_modeling_interval == "Hourly") {
        dframe_pred <- dframe_pred %>%
          select(-c("time", "temp"))
      } else if (model_input_options$chosen_modeling_interval == "Daily") {
        dframe_pred <- dframe_pred %>%
          select(-c("time", "temp", "HDD", "CDD"))
      }

      ok_tow_pred <- factor(ftow) %in% amod$xlevels$ftow
      pred_vec <- rep(NA, length(prediction_data$time))
      pred_vec[ok_tow_pred] <- predict(amod, dframe_pred)

    }


  } else { # run TOWT model

    ok_load <- !is.na(training_data$eload)

    # Determine occupancy information

    occ_info <- find_occ_unocc(interval_of_week[ok_load],
                               training_data$eload[ok_load], training_data$temp[ok_load])
    occ_intervals <- occ_info[occ_info[, 2] == 1, 1]

    # which time intervals are 'occupied'?

    occ_vec <- rep(0, length(training_data$eload))

    if (length(occ_intervals) > 2) {
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }
    }

    # Create temperature matrix
    temp_mat <- create_temp_matrix(training_data$temp, model_input_options$calculated_temp_knots)
    temp_m_name <- rep(NA, ncol(temp_mat))
    for (i in 1 : ncol(temp_mat)) {
      temp_m_name[i] <- paste("temp_mat", i, sep = "")
    }
    names(temp_mat) <- temp_m_name

    ftow <- factor(interval_of_week)
    dframe <- data.frame(training_data, ftow, temp_mat)

    if(model_input_options$chosen_modeling_interval == "Hourly") {
      dframe <- dframe %>%
        select(-c("time", "temp"))
    } else if (model_input_options$chosen_modeling_interval == "Daily") {
      dframe <- dframe %>%
        select(-c("time", "temp", "HDD", "CDD"))
    }

    training_load_pred <- rep(NA, nrow(dframe))

    # create subsets by occupancy
    ok_occ <- occ_vec == 1
    ok_occ[is.na(ok_occ)] <- TRUE

    # make data frame for explanatory variables in prediction period
    if(! is.null(prediction_data)) {

      temp_mat_pred <- create_temp_matrix(prediction_data$temp, model_input_options$calculated_temp_knots)
      names(temp_mat_pred) <- temp_m_name
      # should the temperature knots be recalculated for the prediction dataframe?

      ftow <- factor(interval_of_week_pred)
      dframe_pred <- data.frame(prediction_data, ftow, temp_mat_pred)

      if(model_input_options$chosen_modeling_interval == "Hourly") {
        dframe_pred <- dframe_pred %>%
          select(-c("time", "temp"))
      } else if (model_input_options$chosen_modeling_interval == "Daily") {
        dframe_pred <- dframe_pred %>%
          select(-c("time", "temp", "HDD", "CDD"))
      }

      pred_vec <- rep(NA, length(prediction_data$time))

    }

    # Now make predictions for prediction period
    if (sum(ok_occ > 0)) {

      # linear regression - subset for occupied periods
      amod <- lm(training_data$eload ~ . , data = dframe,
                 na.action = na.exclude, weights = model_input_options$train_weight_vec, subset = ok_occ)
      t_p <- predict(amod, dframe[ok_occ, ])
      training_load_pred[ok_occ] <- t_p

      # filter out times of week that are not in occupied training period.
      if(! is.null(prediction_data)) {
        ok_tow_pred <- dframe_pred$ftow %in% amod$xlevels$ftow
        pred_vec[ok_tow_pred] <- predict(amod, dframe_pred[ok_tow_pred, ])
      }

    }

    if (sum(! ok_occ) > 0) {

      # linear regression - subset for unoccupied periods
      bmod <- lm(training_data$eload ~ . , data = dframe, na.action = na.exclude,
                 weights = model_input_options$train_weight_vec, subset = ! ok_occ)
      t_p <- predict(bmod, dframe[! ok_occ, ])
      training_load_pred[! ok_occ] <- t_p

      # filter out times of week that are not in unoccupied training period.
      if(! is.null(prediction_data)) {
        ok_tow_pred <- dframe_pred$ftow %in% bmod$xlevels$ftow
        pred_vec[ok_tow_pred] <- predict(bmod, dframe_pred[ok_tow_pred, ])

      }

    }
  }

  output <- NULL
  output$training <- data.frame(training_data, training_load_pred)

  if(! is.null(prediction_data)) {
    pred_vec[pred_vec < 0] <- 0
    output$predictions <- data.frame(prediction_data, pred_vec)
  }

  if(model_input_options$regression_type == "TOW") {
    output$model_occupied <- amod
  } else {
    if(sum(ok_occ) > 0) {
      output$model_occupied <- amod
    }

    if(sum(! ok_occ) > 0) {
      output$model_unoccupied <- bmod
    }
  }

  return(output)

}
