#' Generate an energy data model using simple linear regression with outside air temperature.
#'
#' \code{This function builds an energy use model using the simple linear regression algorithm.}
#'
#' @param baseline_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param has_operating_modes Boolean specifying whether the energy use profile has varying operating modes. Only used with 'Simple Linear Regression with OAT'.
#' @param train_operating_mode_data dataframe with indicator variables for the various operating modes present in the model training period.
#' Only used with 'Simple Linear Regression with OAT'.
#' @param pred_operating_mode_data dataframe with indicator variables for the various operating modes present in the mdoel prediction period.
#' Only used with 'Simple Linear Regression with OAT'.
#' @param data_units energy data's units
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{baseline_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls.}
#'   \item{SLR_model}{an object with parameter coefficients and associated p-values resulting from the SLR model.}
#'   \item{normality metrics}{a list with details on residual heteroskedasticity and kurtosis.}
#'   \item{energy use summary}{Summed basesline, post-implementation, and adjusted baseline energy use values.}
#'   \item{model}{the lm object created within 'model_with_SLR'.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export


model_with_SLR <- function(baseline_data, prediction_data = NULL,
                                       data_interval = NULL,
                                       has_operating_modes = FALSE,
                                       train_operating_mode_data = NULL,
                                       pred_operating_mode_data = NULL,
                                       data_units = NULL){

  data_time_interval <- difftime(baseline_data$time[2], baseline_data$time[1], units = "min")

  if (data_time_interval > 60 && data_interval == "Hourly"){
    return(NULL)
  } else {

  if (is.null(train_operating_mode_data)) {
    baseline_data <- baseline_data
    baseline_data <- dplyr::distinct(baseline_data)

    if (is.null(prediction_data)) {
      prediction_data <- NULL
    } else {
      prediction_data <- prediction_data
    }

    if (data_interval == "Hourly") {
      linregress <- lm(eload ~ . - time, baseline_data)
    } else if (data_interval == "Daily") {
      linregress <- lm(eload ~ . - time - HDD - CDD, baseline_data)
    } else if (data_interval == "Monthly") {
      linregress <- lm(eload ~ . - time - HDD - CDD - days - HDDperday - CDDperday - eloadperday, baseline_data)
    }

  } else {

    baseline_data <- dplyr::inner_join(baseline_data, train_operating_mode_data, by = "time")
    baseline_data <- dplyr::distinct(baseline_data)

    # pred read and preprocessing

    if (! is.null(prediction_data)){
      prediction_data <- dplyr::inner_join(prediction_data, pred_operating_mode_data, by = "time")
      prediction_data <- dplyr::distinct(prediction_data)
    } else {
      prediction_data <- baseline_data
    }

    if (data_interval == "Hourly") {
      linregress <- lm(eload ~ . - time, baseline_data)
    } else if (data_interval == "Daily") {
      linregress <- lm(eload ~ . - time - HDD - CDD, baseline_data)
    } else if (data_interval == "Monthly") {
      if (has_operating_modes) {
        return(NULL)
      } else {
      linregress <- lm(eload ~ . - time - HDD - CDD - days - HDDperday - CDDperday - eloadperday, baseline_data)
      }
    }
  }

  intercept <- linregress[[1]][[1]]
  m_temp <- linregress[[1]][[2]]
  r_value <- summary(linregress)$r.squared
  nparameter <- nrow(as.data.frame(linregress$coefficients))

  baseline_data <- baseline_data %>%
    mutate(yfit = predict(linregress, baseline_data)) %>%
    mutate(resi = eload - yfit) %>%
    mutate(resi_sq = resi ^ 2)

  baseline_data <- baseline_data[complete.cases(baseline_data), ]

  MBE <- sum(baseline_data[['resi']]) / nrow(baseline_data)
  NDBE <- 100 * (sum(baseline_data[['resi']] / sum(baseline_data[['eload']])))
  CVRMSE <- ((sqrt(sum(baseline_data[['resi_sq']]) / (nrow(baseline_data) - nparameter))) / mean(baseline_data[['eload']]))
  CVMAE <- (sum(abs(baseline_data[['resi']])) / (nrow(baseline_data) - nparameter)) / mean(baseline_data[['eload']])

  goodness_of_fit <- data.frame()
  goodness_of_fit[1, 1] <- r_value
  goodness_of_fit[1, 2] <- CVRMSE
  goodness_of_fit[1, 3] <- paste(formatC(NDBE, format = "e", digits = 2), "%", sep = "")
  goodness_of_fit[1, 4] <- nparameter

  colnames(goodness_of_fit) <- c("fit_R2", "fit_CVRMSE", "fit_NDBE", "#Parameters")

  parameters <- as.data.frame(rownames(summary(linregress)$coefficients))
  coefficients <- as.data.frame(summary(linregress)$coefficients[, 1])
  p_values <- as.data.frame(summary(linregress)$coefficients[, 4])

  SLR_model <- dplyr::bind_cols(parameters, coefficients, p_values)
  names(SLR_model) <- c("Parameters", "Coefficients", "p-values")

  out <- NULL
  out$goodness_of_fit <- goodness_of_fit
  out$baseline_data <- baseline_data
  out$SLR_model <- SLR_model

  skewness_rsdl <- moments::skewness(baseline_data$resi)
  excess_kurtosis_rsdl <- moments::kurtosis(baseline_data$resi)

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
  names(normality_metrics) <- c("Skewness Value", "Skewness Interpretation",
                                    "Excess Kurtosis Value", "Excess Kurtosis Interpretation")
  normality_metrics$'Skewness Value' <-  formatC(skewness_rsdl, big.mark = ",")
  normality_metrics$'Skewness Interpretation' <- skewness_rsdl_meaning
  normality_metrics$'Excess Kurtosis Value' <-  formatC(excess_kurtosis_rsdl, big.mark = ",")
  normality_metrics$'Excess Kurtosis Interpretation' <- excess_kurtosis_rsdl_meaning

  out$normality_metrics <- normality_metrics

  energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 1))
  names(energy_use_summary) <- c("Baseline Energy Use")
  energy_use_summary$'Baseline Energy Use' <- paste(format(sum(baseline_data$eload, na.rm = T),
                                                             scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

  out$energy_use_summary <- energy_use_summary

  out$model <- linregress

  if (! is.null(prediction_data)) {
    predicted_data <- prediction_data %>%
      mutate(pred_eload =  predict(linregress, prediction_data))

    predicted_data <- predicted_data[complete.cases(predicted_data), ]
    out$post_implementation_data <- predicted_data

    energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 3))
    names(energy_use_summary) <- c("Baseline Energy Use", "Adjusted Baseline Energy Use", "Post Implementation Energy Use")

    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(baseline_data$eload, na.rm = T),
                                                               scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
    energy_use_summary$'Adjusted Baseline Energy Use' <- paste(format(sum(predicted_data$pred_eload, na.rm = T),
                                                                        scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
    energy_use_summary$'Post Implementation Energy Use' <- paste(format(sum(prediction_data$eload, na.rm = T),
                                                                          scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

    out$energy_use_summary <- energy_use_summary
  }
    }

  return(out)
  }
