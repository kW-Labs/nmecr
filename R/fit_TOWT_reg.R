#' Utility function for model_with_TOWT
#'
#' \code{This function builds an energy use/demand models separated by occupancy and temperature regimens.}
#'
#' @param training_list List with training dataframe and operating mode dataframe. Output from create_dataframe
#' @param prediction_list List with prediction dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model_occupied} {an lm object generated when using Time-of-Week and Time-of-Week & Temperature algorithms}
#'   \item{model_unoccupied} {an lm object generated when using TOWT algorithm when the unoccupied period is discernably different from the occupied period}
#'   \item{training_data} {training dataframe along with the model_fit values}
#'   \item{prediction_data} {prediction dataframe along with the model prediction values. Only generated when prediction_list is supplied to the algorithm}
#' }

fit_TOWT_reg <- function(training_list = NULL, prediction_list = NULL, model_input_options = NULL) {


  # prepare interval of week for training data
  minute_of_week <- (lubridate::wday(training_list$dataframe$time) - 1) * 24 * 60 +
    lubridate::hour(training_list$dataframe$time) * 60 + lubridate::minute(training_list$dataframe$time)

  interval_of_week <- 1 + floor(minute_of_week / model_input_options$interval_minutes)

  # prepare interval of week for prediction data
  if(! is.null(prediction_list)) {

    minute_of_week_pred <- (lubridate::wday(prediction_list$dataframe$time) - 1) * 24 * 60 +
      lubridate::hour(prediction_list$dataframe$time) * 60 + lubridate::minute(prediction_list$dataframe$time)

    interval_of_week_pred <- 1 + floor(minute_of_week_pred / model_input_options$interval_minutes)
  }

  # run Time-of-Week model

  if (model_input_options$regression_type == "TOW") {

    ftow <- factor(interval_of_week)
    dframe <- data.frame("time" = training_list$dataframe$time, ftow)

    # add training operating mode data if available
    if (! is.null(training_list$operating_mode_data)) {
      dframe <- dplyr::inner_join(dframe, training_list$operating_mode_data, by = "time")
    }

    dframe <- dframe %>%
      select(-"time")

    # simple linear regression - no subsetting by occupancy
    amod <- lm(training_list$dataframe$eload ~ . , data = dframe, na.action = na.exclude, weights = model_input_options$train_weight_vec)
    training_load_pred <- predict(amod)

    # make predictions
    if(! is.null(prediction_list)) {

      ftow <- factor(interval_of_week_pred)
      dframe_pred <- data.frame("time" = prediction_list$dataframe$time, ftow)

      # add prediction operating mode data if available
      if (! is.null(prediction_list$operating_mode_data)) {
        dframe_pred <- dplyr::inner_join(dframe_pred, prediction_list$operating_mode_data, by = "time")
      }

      dframe_pred <- dframe_pred %>%
        select(-"time")

      ok_tow_pred <- factor(ftow) %in% amod$xlevels$ftow
      pred_vec <- rep(NA, length(prediction_list$dataframe$time))
      pred_vec[ok_tow_pred] <- predict(amod, dframe_pred)

    }


  } else { # run TOWT model

    ok_load <- !is.na(training_list$dataframe$eload)

    # Determine occupancy information

    occ_info <- find_occ_unocc(interval_of_week[ok_load],
                               training_list$dataframe$eload[ok_load], training_list$dataframe$temp[ok_load])
    occ_intervals <- occ_info[occ_info[, 2] == 1, 1]

    # which time intervals are 'occupied'?

    occ_vec <- rep(0, length(training_list$dataframe$eload))

    if (length(occ_intervals) > 2) {
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }
    }

    # Create temperature matrix
    temp_mat <- create_temp_matrix(training_list$dataframe$temp, model_input_options$calculated_temp_knots)
    temp_m_name <- rep(NA, ncol(temp_mat))
    for (i in 1 : ncol(temp_mat)) {
      temp_m_name[i] <- paste("temp_mat", i, sep = "")
    }
    names(temp_mat) <- temp_m_name

    ftow <- factor(interval_of_week)
    dframe <- data.frame("time" = training_list$dataframe$time, ftow, temp_mat)

    # add training operating mode data if available
    if (! is.null(training_list$operating_mode_data)) {
      dframe <- dplyr::inner_join(dframe, training_list$operating_mode_data, by = "time")
    }

    dframe <- dframe %>%
      select(-"time")

    training_load_pred <- rep(NA, nrow(dframe))

    # create subsets by occupancy
    ok_occ <- occ_vec == 1
    ok_occ[is.na(ok_occ)] <- TRUE

    # make data frame for explanatory variables in prediction period
    if(! is.null(prediction_list)) {

      temp_mat_pred <- create_temp_matrix(prediction_list$dataframe$temp, model_input_options$calculated_temp_knots)
      names(temp_mat_pred) <- temp_m_name
      # should the temperature knots be recalculated for the prediction dataframe?

      ftow <- factor(interval_of_week_pred)
      dframe_pred <- data.frame("time" = prediction_list$dataframe$time, ftow, temp_mat_pred)

      # add prediction operating mode data if available
      if (! is.null(prediction_list$operating_mode_data)) {
        dframe_pred <- dplyr::inner_join(dframe_pred, prediction_list$operating_mode_data, by = "time")
      }

      dframe_pred <- dframe_pred %>%
        select(-"time")

      pred_vec <- rep(NA, length(prediction_list$dataframe$time))

    }

    # Now make predictions for prediction period
    if (sum(ok_occ > 0)) {

      # linear regression - subset for occupied periods
      amod <- lm(training_list$dataframe$eload ~ . , data = dframe,
                 na.action = na.exclude, weights = model_input_options$train_weight_vec, subset = ok_occ)
      t_p <- predict(amod, dframe[ok_occ, ])
      training_load_pred[ok_occ] <- t_p

      # filter out times of week that are not in occupied training period.
      if(! is.null(prediction_list)) {
        ok_tow_pred <- dframe_pred$ftow %in% amod$xlevels$ftow
        pred_vec[ok_tow_pred] <- predict(amod, dframe_pred[ok_tow_pred, ])
      }

    }

    if (sum(! ok_occ) > 0) {

      # linear regression - subset for unoccupied periods
      bmod <- lm(training_list$dataframe$eload ~ . , data = dframe, na.action = na.exclude,
                 weights = model_input_options$train_weight_vec, subset = ! ok_occ)
      t_p <- predict(bmod, dframe[! ok_occ, ])
      training_load_pred[! ok_occ] <- t_p

      # filter out times of week that are not in unoccupied training period.
      if(! is.null(prediction_list)) {
        ok_tow_pred <- dframe_pred$ftow %in% bmod$xlevels$ftow
        pred_vec[ok_tow_pred] <- predict(bmod, dframe_pred[ok_tow_pred, ])

      }

    }
  }

  output <- NULL
  output$training <- data.frame(training_list$dataframe, training_load_pred)

  if(! is.null(prediction_list)) {
    pred_vec[pred_vec < 0] <- 0
    output$predictions <- data.frame(prediction_list$dataframe, pred_vec)
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
