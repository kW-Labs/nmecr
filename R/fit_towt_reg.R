#' Develop linear regression models for energy use data using time-of-week and outside air temperature as independent variables.
#'
#' \code{This function generates linear regressions for energy use data using the temperature changepoints and occupancy schedules determined
#' by two other kWMV functions: 'find_occ_unocc' and 'create_temp_matrix'.
#' It is a utility function for another kWMV function: 'create_towt_weighted_reg'.
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

fit_towt_reg <- function(time_col, eload_col, temp_col,
                           pred_time_col, pred_temp_col, temp_knots=temp_knots,
                           weight_vec=weight_vec,
                           interval_minutes=interval_minutes,
                           run_temperature_model=run_temperature_model,
                           has_operating_modes = has_operating_modes,
                          train_operating_mode_data = train_operating_mode_data,
                          pred_operating_mode_data = pred_operating_mode_data,
                           categories = categories) {

  minute_of_week <- (lubridate::wday(time_col) - 1) * 24 * 60 +
    lubridate::hour(time_col) * 60 + lubridate::minute(time_col)
  interval_of_week <- 1 + floor(minute_of_week / interval_minutes)
  num_load_time <- as.numeric(time_col)

  minute_of_week_pred <- (lubridate::wday(pred_time_col) - 1) * 24 * 60 +
    lubridate::hour(pred_time_col) * 60 + lubridate::minute(pred_time_col)
  interval_of_week_pred <- 1 + floor(minute_of_week_pred / interval_minutes)
  num_pred_time_col <- as.numeric(pred_time_col)

  if (is.null(temp_col) | !run_temperature_model) {

    ftow <- factor(interval_of_week)
    dframe <- data.frame(ftow)

    if (has_operating_modes) {
      dframe <- dplyr::bind_cols(dframe, train_operating_mode_data)
    } else {
      dframe <- dframe
    }

    amod <- lm(eload_col ~ . + 0, data = dframe, na.action = na.exclude, weights = weight_vec)
    training_load_pred <- predict(amod)

    ftow <- factor(interval_of_week_pred)
    dframe_pred <- data.frame(ftow)

    if (has_operating_modes) {
      dframe_pred <- dplyr::bind_cols(dframe_pred, pred_operating_mode_data)
    } else {
      dframe_pred <- dframe_pred
    }

    ok_tow_pred <- factor(ftow) %in% amod$xlevels$ftow
    pred_vec <- rep(NA, length(pred_time_col))
    pred_vec[ok_tow_pred] <- predict(amod, dframe_pred)

  } else {

    ok_load <- ! is.na(eload_col)
    occ_info <- find_occ_unocc(interval_of_week[ok_load],
                               eload_col[ok_load], temp_col[ok_load])
    occ_intervals <- occ_info[occ_info[, 2] == 1, 1]
    # which time intervals are 'occupied'?

    occ_vec <- rep(0, length(eload_col))
    if (length(occ_intervals) > 2) {
      for (i in 1 : length(occ_intervals)) {
        occ_vec[interval_of_week == occ_intervals[i]] <- 1
      }
    }

    # If there aren't enough temperature data above the highest temp knot,
    # then remove the knot.
    # Repeat until there are sufficient data above the highest
    # remaining knot, or until there's only one knot left.

    num_temp_knots <- length(temp_knots)

    check_knots <- TRUE
    while (check_knots) {
      if (sum(temp_col[ok_load] > temp_knots[num_temp_knots],
              na.rm = TRUE) < 20) {
        # not enough data above upper knot; throw away that upper knot
        temp_knots <- temp_knots[- num_temp_knots]
        num_temp_knots <- num_temp_knots - 1
        if (num_temp_knots == 1) {
          check_knots <- FALSE
        }
      } else {
        # We have enough data above the upper knot, so need to keep checking
        check_knots <- FALSE
      }
    }

    # Same principle as above, for aomount of data below the lowest knot.
    check_knots <- TRUE
    while (check_knots) {
      if (sum(temp_col[ok_load] < temp_knots[1], na.rm = TRUE) < 20) {
        # not enough data below lower knot; throw away that lower knot
        temp_knots <- temp_knots[- 1]
        num_temp_knots <- num_temp_knots - 1
        if (num_temp_knots == 1) {
          # We have to keep one knot, even though we have no data below it.
          check_knots <- FALSE
        }
      } else {
        check_knots <- FALSE # we have sufficient data below the lowest knot
      }
    }

    temp_mat <- create_temp_matrix(temp_col, temp_knots)
    temp_mat_pred <- create_temp_matrix(pred_temp_col, temp_knots)
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

    if (has_operating_modes) {
      dframe <- dplyr::bind_cols(dframe, train_operating_mode_data)
    } else {
      dframe <- dframe
    }

    training_load_pred <- rep(NA, nrow(dframe))

    # make data frame for explanatory variables in prediction period
    ftow <- factor(interval_of_week_pred)
    dframe_pred <- data.frame(ftow, temp_mat_pred)

    if (has_operating_modes) {
      dframe_pred <- bind_cols(dframe_pred, pred_operating_mode_data)
    } else {
      dframe_pred <- dframe_pred
    }

    pred_vec <- rep(NA, length(pred_time_col))

    ok_occ <- occ_vec == 1
    ok_occ[is.na(ok_occ)] <- TRUE

    # Now make predictions for prediction period
    if (sum(ok_occ > 0)) {

      # fit model to training data
      amod <- lm(eload_col ~ . + 0, data = dframe,
                 na.action = na.exclude, weights = weight_vec, subset = ok_occ)
      t_p <- predict(amod, dframe[ok_occ, ])
      training_load_pred[ok_occ] <- t_p

      # filter out times of week that are not in occupied training period.
      ok_tow_pred <- dframe_pred$ftow %in% amod$xlevels$ftow
      pred_vec[ok_tow_pred] <- predict(amod, dframe_pred[ok_tow_pred, ])

    }

    if (sum(! ok_occ) > 0) {

      bmod <- lm(eload_col ~ . + 0, data = dframe, na.action = na.exclude,
                 weights = weight_vec, subset = ! ok_occ)
      t_p <- predict(bmod, dframe[! ok_occ, ])
      training_load_pred[! ok_occ] <- t_p

      # filter out times of week that are not in unoccupied training period.
      ok_tow_pred <- dframe_pred$ftow %in% bmod$xlevels$ftow
      pred_vec[ok_tow_pred] <- predict(bmod, dframe_pred[ok_tow_pred, ])

    }
  }

  pred_vec[pred_vec < 0] <- 0


  output <- NULL
  output$training <- data.frame(time_col, num_load_time, training_load_pred)
  output$predictions <- data.frame(pred_time_col, num_pred_time_col, pred_vec)

  if (run_temperature_model) {

    if (sum(ok_occ > 0)) {

      parameters <- as.data.frame(rownames(summary(amod)$coefficients))
      coefficients <- as.data.frame(summary(amod)$coefficients[, 1])
      p_values <- as.data.frame(summary(amod)$coefficients[, 4])

      lm_results_occ_period <- dplyr::bind_cols(parameters, coefficients,
                                                       p_values)
      names(lm_results_occ_period) <- c("Parameters", "Coefficients",
                                               "p-values")
      output$lm_results_occ_period <- lm_results_occ_period

    } else {
      output$lm_results_occ_period <- "No occupied mode model available"
    }

    if (sum(! ok_occ) > 0) {

      parameters <- as.data.frame(rownames(summary(bmod)$coefficients))
      coefficients <- as.data.frame(summary(bmod)$coefficients[, 1])
      p_values <- as.data.frame(summary(bmod)$coefficients[, 4])

      lm_results_unocc_period <- dplyr::bind_cols(parameters, coefficients,
                                                     p_values)
      names(lm_results_unocc_period) <- c("Parameters", "Coefficients",
                                             "p-values")
      output$lm_results_unocc_period <- lm_results_unocc_period

    } else {
      output$lm_results_unocc_period <- "No unoccupied mode model available"
    }


  } else {

    parameters <- as.data.frame(rownames(summary(amod)$coefficients))
    coefficients <- as.data.frame(summary(amod)$coefficients[, 1])
    p_values <- as.data.frame(summary(amod)$coefficients[, 4])

    lm_results_occ_period <- dplyr::bind_cols(parameters, coefficients,
                                                   p_values)
    names(lm_results_occ_period) <- c("Parameters", "Coefficients",
                                           "p-values")
    output$lm_results_occ_period <- lm_results_occ_period

  }

  return(output)
}
