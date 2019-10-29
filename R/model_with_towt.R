#' Generate an energy data model using the Time-of-Week and Temperature algorithm.
#'
#' \code{This function builds an energy use model using two algorithms: TOWT and MW.
#' This function is adapted from work by LBNL: \url{https://lbnl-eta.github.io/RMV2.0/}}
#'
#' @param training_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param timescale_days Numeric correspond to the timescale for weighting function.Default: NULL.
#' Change to improve accuracy of short term models.
#' @param interval_minutes Numeric for the interval period. Default: 15.
#' @param has_temp_knots_defined Boolean specifying whether the temp_knots are pre-defined or will be calculated by the algorithm
#' @param run_temperature_model Boolean specifying whether temperature should or should not be used in regression modeling.
#' @param equal_temp_segment_points Boolean specifying structure of temperature segments: equal number of points vs. equal segment length
#' @param temp_segments_numeric Numeric for number of temperature segments. Default: 6
#' @param temp_knots_value Vector specifying manually defined temperature knots.
#' @param has_operating_modes Boolean specifying whether the energy use profile has varying operating modes.
#' @param train_operating_mode_data dataframe with indicator variables for the various operating modes present in the model training period.
#' @param pred_operating_mode_data dataframe with indicator variables for the various operating modes present in the mdoel prediction period.
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param data_units energy data's units
#'
#' @return a TOWT_baseline object, which is a list with the following components:
#' \describe{
#'   \item{TOWT_model}{an object that has been created by the function create_TOWT_weighted_reg,
#'    and which correspond to the TOWT model.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls}
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{rsdl}{a data frame containing all residual values}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export

model_with_TOWT <- function(training_data = NULL,
                          prediction_data = NULL,
                          timescale_days = NULL,
                          interval_minutes = 15,
                          has_temp_knots_defined = FALSE,
                          run_temperature_model = TRUE,
                          equal_temp_segment_points = T,
                          temp_segments_numeric = 6,
                          temp_knots_value = c(40, 45, 50, 60, 65, 90),
                          has_operating_modes = FALSE,
                          train_operating_mode_data = NULL,
                          pred_operating_mode_data = NULL,
                          data_interval = NULL,
                          data_units = NULL){

<<<<<<< Updated upstream
  if (! has_operating_modes){

    train <- training_data
    train <- dplyr::distinct(train)

    train_operating_mode_data <- NULL
    pred_operating_mode_data <- NULL

    # pred read and preprocessing
    if (! is.null(prediction_data)) {
      pred <- prediction_data
      pred <- dplyr::distinct(pred)
    } else {
      pred <- train
    }

    if (! has_temp_knots_defined) {
      temp_segments <- temp_segments_numeric
    } else {
      temp_segments <- length(temp_knots_value)
    }

    eload_time <- train$time
    eload_time_interval <- difftime(eload_time[2], eload_time[1], units = "min")

    if (eload_time_interval > 60 && data_interval == "Hourly") {
      return(NULL)
    } else {

      TOWT_model <- create_TOWT_weighted_reg(train$time,
                                train$eload,
                                train$temp,
                                pred$time,
                                pred$temp,
                                timescale_days = timescale_days,
                                interval_minutes = interval_minutes,
                                has_temp_knots_defined = has_temp_knots_defined,
                                run_temperature_model = run_temperature_model,
                                equal_temp_segment_points = equal_temp_segment_points,
                                temp_segments = temp_segments_numeric,
                                temp_knots_value = temp_knots_value,
                                has_operating_modes = has_operating_modes,
                                train_operating_mode_data = train_operating_mode_data,
                                pred_operating_mode_data = pred_operating_mode_data,
                                categories = categories)
    }

  } else {

    eload_time <- training_data$time
    op_time <- train_operating_mode_data$time

    eload_time_interval <- difftime(eload_time[2], eload_time[1], units = "min")
    op_time_interval <- difftime(op_time[2], op_time[1], units = "min")

    if (op_time_interval == 1440 && eload_time_interval == 60 && data_interval == "Hourly" ||
        op_time_interval > 1440 && eload_time_interval == 60 && data_interval == "Hourly" ||
        eload_time_interval > 60 && data_interval == "Hourly" ||
        data_interval == "Monthly") {

      return (NULL)

    } else {

      #categories' names

      categories <- as.vector(colnames(train_operating_mode_data))
      categories <- categories[-1]

      # train read and preprocess

      train <- dplyr::inner_join(training_data, train_operating_mode_data, by = "time")
      train <- dplyr::distinct(train)

      if (! is.null(prediction_data)){
        pred <- dplyr::inner_join(prediction_data, pred_operating_mode_data, by = "time")
        pred <- dplyr::distinct(pred)
      } else {
        pred <- train
      }

      if (! has_temp_knots_defined) {
        temp_segments <- temp_segments_numeric
      } else {
        temp_segments <- length(temp_knots_value)
      }

      train_operating_mode_data <- as.data.frame(train[, categories])
      pred_operating_mode_data <- as.data.frame(pred[, categories])

      TOWT_model <- create_TOWT_weighted_reg(train$time,
                                train$eload,
                                train$temp,
                                pred$time,
                                pred$temp,
                                timescale_days = timescale_days,
                                interval_minutes = interval_minutes,
                                has_temp_knots_defined = has_temp_knots_defined,
                                run_temperature_model = run_temperature_model,
                                equal_temp_segment_points = equal_temp_segment_points,
                                temp_segments = temp_segments,
                                temp_knots_value = temp_knots_value,
                                has_operating_modes = has_operating_modes,
                                train_operating_mode_data = train_operating_mode_data,
                                pred_operating_mode_data = pred_operating_mode_data,
                                categories = categories)

    }
=======
  # pred read and preprocessing ----
  if (! is.null(prediction_data)) {
    prediction_data <- prediction_data %>%
      dplyr::distinct(pred)
  } else {
    prediction_data <- training_data
>>>>>>> Stashed changes
  }

  # calculate temperature knots ----
  temp_knots <- calculate_temperature_knots(training_data = training_data, has_temp_knots_defined = has_temp_knots_defined,
                                            temp_knots_value = temp_knots_value, temp_segments_numeric = temp_segments_numeric,
                                            equal_temp_segment_points = equal_temp_segment_points)

<<<<<<< Updated upstream
    # Fitting results:
    if (run_temperature_model == T) {

      if (class(TOWT_model$training_model_occ_period) == "character"){
        occ_coeff_count <- 0
      } else {
        occ_coeff_count <- nrow(TOWT_model$training_model_occ_period)
      }
=======
  # create and extract weighting vectors for training and prediction dataframes as per timescale_days ----
  weighted_regressions <- create_weighted_regressions(training_data = training_data, prediction_data = prediction_data,
                                                      timescale_days = timescale_days, interval_minutes = interval_minutes,
                                                      run_temperature_model = run_temperature_model, temp_knots = temp_knots)

>>>>>>> Stashed changes

      if (class(TOWT_model$training_model_unocc_period) == "character"){
          unocc_coeff_count <- 0
      } else {
      unocc_coeff_count <- nrow(TOWT_model$training_model_unocc_period)
      }

<<<<<<< Updated upstream
     nparameter <- occ_coeff_count +  unocc_coeff_count

    } else {

      if (class(TOWT_model$training_model_occ_period)  == "character"){
      occ_coeff_count <- 0
      } else {
      occ_coeff_count <- nrow(TOWT_model$training_model_occ_period)
      }

      nparameter <- occ_coeff_count

    }

    yfit <- TOWT_model$final_train_matrix
    train_output <- train$eload
    fit_residuals_numeric <- train_output - yfit
    fit_residuals <- as.data.frame(train_output - yfit)

    # R-sqaured (Absolute)
    fit_r2 <- (1 - mean((fit_residuals_numeric) ^ 2, na.rm = T) / var(train_output))

    # Mean Sqaured Error (Absolute)
    fit_mse <- ((sum(fit_residuals^2, na.rm = T)) / ((nrow(fit_residuals) - nparameter)))

    # Root Mean Sqaured Error (Absolute)
    fit_rmse <- (sqrt((sum(fit_residuals^2, na.rm = T))/((nrow(fit_residuals) - nparameter))))

    # Coefficient of Variation of the Root Mean Square Error (Absolute)
    fit_cvrmse <- (sqrt((sum(fit_residuals^2, na.rm = T)) / ((nrow(fit_residuals) - nparameter))))/mean(train_output, na.rm = T)

    # Mean Bias Error (Absolute)
    mbe <- sum(fit_residuals_numeric, na.rm = T) / nrow(fit_residuals)

    # Coefficient of Variation of Mean Absolute error (Absolute)
    cvmae <- (sum(abs(fit_residuals_numeric), na.rm = T) / nrow(fit_residuals))/mean(train_output, na.rm = T)

    # Net Determination Bias Error (Percentage)
    fit_ndbe <- 100*(sum(fit_residuals_numeric, na.rm = T)) / sum(train_output, na.rm = T)

    goodness_of_fit <- as.data.frame(matrix(nr = 1, nc = 4))
    names(goodness_of_fit) <- c("fit_R2", "fit_CVRMSE", "fit_NDBE", "#Parameters")
    goodness_of_fit$fit_R2 <- fit_r2
    goodness_of_fit$fit_CVRMSE <- fit_cvrmse
    goodness_of_fit$fit_NDBE <- paste(formatC(fit_ndbe, format = "e", digits = 2), "%", sep = "")
    goodness_of_fit$'#Parameters' <- nparameter

    res <- NULL
    res$TOWT_model <- TOWT_model
    train$time <- format(train$time, "%m/%d/%y %H:%M")
    res$goodness_of_fit <- goodness_of_fit
    res$training_data <- as.data.frame(cbind(train, yfit, "resi" = fit_residuals_numeric))
    res$rdsl <- fit_residuals

    skewness_rsdl <- moments::skewness(fit_residuals)
    excess_kurtosis_rsdl <- moments::kurtosis(fit_residuals)

    if (skewness_rsdl > - 0.5 && skewness_rsdl < 0.5) {
      skewness_rsdl_meaning <- "Fairly Symmetrical"
    } else if (skewness_rsdl > - 1 && skewness_rsdl < - 0.5 || skewness_rsdl > 0.5 && skewness_rsdl < 1) {
      skewness_rsdl_meaning <- "Moderately Skewed"
    } else {
      skewness_rsdl_meaning <- "Highly Skewed"
    }

    if (excess_kurtosis_rsdl == 0) {
      excess_kurtosis_rsdl_meaning <- "Data has no outliers"
    } else if (excess_kurtosis_rsdl > 0) {
      excess_kurtosis_rsdl_meaning <- "Profusion of outliers"
    } else {
      excess_kurtosis_rsdl_meaning <- "Lack of outliers"
    }

    normality_metrics <- as.data.frame(matrix(nr = 1, nc = 4))
    names(normality_metrics) <- c("Skewness Value", "Skewness",
                                  "Excess Kurtosis Value", "Excess Kurtosis")
    normality_metrics$'Skewness Value' <-  formatC(skewness_rsdl, big.mark = ",")
    normality_metrics$'Skewness' <- skewness_rsdl_meaning
    normality_metrics$'Excess Kurtosis Value' <-  formatC(excess_kurtosis_rsdl, big.mark = ",")
    normality_metrics$'Excess Kurtosis' <- excess_kurtosis_rsdl_meaning

    res$normality_metrics <- normality_metrics

    energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 1))
    names(energy_use_summary) <- "Baseline Energy Use"
    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(train_output, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

    res$energy_use_summary <- energy_use_summary

    # Prediction
    if (! is.null(prediction_data)) {
      t_base_num <- as.numeric(TOWT_model$time_vec)
      final_pred_matrix <- TOWT_model$final_pred_matrix
      y_pred <- approx(t_base_num, final_pred_matrix, as.numeric(pred$time), method = "constant")$y
      pred$time <- format(pred$time, "%m/%d/%y %H:%M")
      res$post_implementation_data <- as.data.frame(pred)
      res$post_implementation_data <- as.data.frame(cbind(pred, "pred_eload" = y_pred))

      energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 3))
      names(energy_use_summary) <- c("Baseline Energy Use", "Adjusted Baseline Energy Use", "Post Implementation Energy Use")

      energy_use_summary$'Baseline Energy Use' <- paste(format(sum(train_output, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Adjusted Baseline Energy Use' <- paste(format(sum(y_pred, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Post Implementation Energy Use' <- paste(format(sum(pred$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

      res$energy_use_summary <- energy_use_summary
    }

    return(res)
=======
>>>>>>> Stashed changes
}
