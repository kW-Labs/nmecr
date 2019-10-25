#' TODO: return model fit and model form only.
#' TODO: create new prediction function - common to all modeled objects
#' Develop linear regression models for energy use data using time-of-week and outside air temperature as independent variables.
#'
#' \code{This function generates linear regressions for energy use data using the temperature changepoints and occupancy schedules determined
#' by two other kWMV functions: 'find_occ_unocc' and 'create_temp_matrix'.
#' It is a utility function for another kWMV function: 'create_TOWT_weighted_reg'.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param time_col time column of training data.
#' @param eload_col eload column of training data.
#' @param temp_col temp column of training data.
#' @param pred_time_col time column of prediction data.
#' @param pred_temp_col temp column of prediction data.
#' @param temp_knots vector specifying temperture knots for linear regression.
#' @param weight_vec vector specifying the weighting factor of time intervals. Default: 1*length of training data.
#' Change to improve accuracy of short term models.
#' @param interval_minutes value defining the interval period. Default: 15.
#' @param run_temperature_model Boolean specifying whether temperature should or should not be used in regression modeling.
#' @param has_operating_modes Boolean specifying whether the energy use profile has varying operating modes.
#' @param train_operating_mode_data dataframe with indicator variables for the various operating modes present in the model training period.
#' @param pred_operating_mode_data dataframe with indicator variables for the various operating modes present in the mdoel prediction period.
#' @param categories vector specifying names of the operating modes present.
#' @return A list of dataframes - training data, predictions, and model coefficients with their statistical significance values
#'
#' @export

fit_TOWT_reg <- function(training_data = NULL, prediction_data = NULL, temp_knots = NULL,
                           train_weight_vec = NULL, interval_minutes = NULL,
                           run_temperature_model = NULL) {

  minute_of_week <- (lubridate::wday(training_data$time) - 1) * 24 * 60 +
    lubridate::hour(training_data$time) * 60 + lubridate::minute(training_data$time)

  interval_of_week <- 1 + floor(minute_of_week / interval_minutes)

  num_load_time <- as.numeric(training_data$time)

  minute_of_week_pred <- (lubridate::wday(prediction_data$time - 1) * 24 * 60 +
    lubridate::hour(prediction_data$time) * 60 + lubridate::minute(prediction_data$time)

  interval_of_week_pred <- 1 + floor(minute_of_week_pred / interval_minutes)

  num_pred_time_col <- as.numeric(prediction_data$time)

  training_operating_mode_data <- training_data %>%
    dplyr::select(-c("time", "eload", "temp"))

  prediction_operating_mode_data <- prediction_data %>%
    dplyr::select(-c("time", "eload", "temp"))

  # run Time-only model

  if (!run_temperature_model) {

    ftow <- factor(interval_of_week)
    dframe <- data.frame(ftow)

    if (!is.null(training_operating_mode_data)) {
      dframe <- dplyr::bind_cols(dframe, train_operating_mode_data)
    } else {
      dframe <- dframe
    }

    amod <- lm(training_data$eload ~ . + 0, data = dframe, na.action = na.exclude, weights = train_weight_vec)
    training_load_pred <- predict(amod)

    ftow <- factor(interval_of_week_pred)
    dframe_pred <- data.frame(ftow)

    if (! is.null(prediction_operating_mode_data)) {
      dframe_pred <- dplyr::bind_cols(dframe_pred, pred_operating_mode_data)
    } else {
      dframe_pred <- dframe_pred
    }

    ok_tow_pred <- factor(ftow) %in% amod$xlevels$ftow
    pred_vec <- rep(NA, length(prediction_data$time)
    pred_vec[ok_tow_pred] <- predict(amod, dframe_pred)

    pred_vec[pred_vec < 0] <- 0

  } else { # run TOWT model

    ok_load <- !is.na(training_data$eload)

    # Determine occupancy information

    occ_info <- find_occ_unocc(interval_of_week[ok_load],
                               training_data$eload[ok_load], training_data$temp[ok_load])
    occ_intervals <- occ_info[occ_info[, 2] == 1, 1]

    # which time intervals are 'occupied'?

    occ_vec <- rep(0, length(training_data$eload)
    if (length(occ_intervals) > 2) {
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }
    }

    # Remove extra temperature knots

    num_temp_knots <- remove_extra_temp_knots(training_data = training_data, temp_knots = temp_knots)

    # Create temperature matrix

    temp_mat <- create_temp_matrix(training_data$temp, temp_knots)
    temp_mat_pred <- create_temp_matrix(prediction_data$temp, temp_knots)
    temp_m_name <- rep(NA, ncol(temp_mat))

    for (i in 1 : ncol(temp_mat)) {
      temp_m_name[i] <- paste("temp_mat", i, sep = "")
    }

    names(temp_mat) <- temp_m_name
    names(temp_mat_pred) <- temp_m_name

    # make data frame for explanatory variables in training period
    # We will use the variable name ftow twice:
    # first for the training period and then for the prediction period,
    # for notational convenience when using	the predict() function.

    ftow <- factor(interval_of_week)
    dframe <- data.frame(ftow, temp_mat)

    if (!is.null(training_operating_mode_data) {
      dframe <- dplyr::bind_cols(dframe, train_operating_mode_data)
    } else {
      dframe <- dframe
    }

    training_load_pred <- rep(NA, nrow(dframe))

    # make data frame for explanatory variables in prediction period
    ftow <- factor(interval_of_week_pred)
    dframe_pred <- data.frame(ftow, temp_mat_pred)

    if (!is.null(prediction_operating_mode_data) {
      dframe_pred <- bind_cols(dframe_pred, pred_operating_mode_data)
    } else {
      dframe_pred <- dframe_pred
    }

    pred_vec <- rep(NA, length(prediction_data$time)

    ok_occ <- occ_vec == 1
    ok_occ[is.na(ok_occ)] <- TRUE

    # Now make predictions for prediction period
    if (sum(ok_occ > 0)) {

      # fit model to training data
      amod <- lm(training_data$eload ~ . + 0, data = dframe,
                 na.action = na.exclude, weights = train_weight_vec, subset = ok_occ)
      t_p <- predict(amod, dframe[ok_occ, ])
      training_load_pred[ok_occ] <- t_p

      # filter out times of week that are not in occupied training period.
      ok_tow_pred <- dframe_pred$ftow %in% amod$xlevels$ftow
      pred_vec[ok_tow_pred] <- predict(amod, dframe_pred[ok_tow_pred, ])

    } if (sum(! ok_occ) > 0) {

      bmod <- lm(training_data$eload ~ . + 0, data = dframe, na.action = na.exclude,
                 weights = train_weight_vec, subset = ! ok_occ)
      t_p <- predict(bmod, dframe[! ok_occ, ])
      training_load_pred[! ok_occ] <- t_p

      # filter out times of week that are not in unoccupied training period.
      ok_tow_pred <- dframe_pred$ftow %in% bmod$xlevels$ftow
      pred_vec[ok_tow_pred] <- predict(bmod, dframe_pred[ok_tow_pred, ])

      pred_vec[pred_vec < 0] <- 0

    }
  }

  output <- NULL
  output$training <- data.frame(training_data$time, num_load_time, training_load_pred)
  output$predictions <- data.frame(prediction_data$time, num_pred_time_col, pred_vec)

  return(output)

}
