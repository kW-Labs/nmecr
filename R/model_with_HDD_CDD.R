#' Generate an energy data model using HDD and CDD models with outside air temperature.
#'
#' \code{This function builds an energy use model using linear regression on computed heating degree days, or cooling degree days, or a combination of the two.}
#'
#' @param training_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param regression_type Character string indictating the modeling algorithm to run: "Three Parameter Heating", "Three Parameter Cooling",
#' "Four Parameter Linear Model", "Five Parameter Linear Model",
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param data_units energy data's units
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls.}
#'   \item{HDD_CDD_model}{an object with parameter coefficients and associated p-values resulting from the HDD_CDD model.}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{model}{the lm object created within 'model_with_CP'.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export
#'

model_with_HDD_CDD <- function(training_data, prediction_data = NULL,
                          regression_type = NULL,
                          data_interval = NULL,
                          data_units = NULL){

  if (regression_type == "HDD-CDD Multivariate Regression") {

    data_time_interval <- difftime(training_data$time[2], training_data$time[1], units = "min")

    if (data_interval == "Monthly") {

    linregress <- lm(training_data[['eloadperday']] ~ training_data[['HDDperday']] + training_data[['CDDperday']])

    intercept <- linregress[[1]][[1]]

    m_HDD <- linregress[[1]][[2]]

    if (is.na(m_HDD)) {
      m_HDD <- 0
    }

    m_CDD <- linregress[[1]][[3]]

    if (is.na(m_CDD)) {
      m_CDD <- 0
    }

    r_value <- summary(linregress)$r.squared

    training_data <- training_data %>%
      mutate(yfit_perday = training_data[['HDDperday']] * m_HDD + training_data[['CDDperday']] * m_CDD + intercept) %>%
      mutate(resi = eloadperday - yfit_perday) %>%
      mutate(resi_sq = resi ^ 2) %>%
      mutate(yfit = yfit_perday * days)

    training_data <- training_data[complete.cases(training_data), ]

    nparameter <- nrow(as_data_frame(linregress$coefficients))

    MBE <- sum(training_data[['resi']]) / nrow(training_data)
    NDBE <- 100 * (sum(training_data[['resi']] / sum(training_data[['eloadperday']])))
    CVRMSE <- ((sqrt(sum(training_data[['resi_sq']]) / (nrow(training_data) - nparameter))) / mean(training_data[['eloadperday']]))
    CVMAE <- (sum(abs(training_data[['resi']])) / (nrow(training_data) - nparameter)) / mean(training_data[['eloadperday']])

    goodness_of_fit <- data.frame()
    goodness_of_fit[1, 1] <- r_value
    goodness_of_fit[1, 2] <- CVRMSE
    goodness_of_fit[1, 3] <- paste(formatC(NDBE, format = "e", digits = 2), "%", sep = "")
    goodness_of_fit[1, 4] <- nparameter

    colnames(goodness_of_fit) <- c("fit_R2", "fit_CVRMSE", "fit_NDBE", "#Parameters")

    parameters <- as.data.frame(rownames(summary(linregress)$coefficients))
    coefficients <- as.data.frame(summary(linregress)$coefficients[, 1])
    p_values <- as.data.frame(summary(linregress)$coefficients[, 4])

    HDD_CDD_model <- bind_cols(parameters, coefficients, p_values)
    names(HDD_CDD_model) <- c("Parameters", "Coefficients", "p-values")

    out <- NULL
    out$goodness_of_fit <- goodness_of_fit
    out$training_data <- training_data
    out$HDD_CDD_model <- HDD_CDD_model

    skewness_rsdl <- moments::skewness(training_data$resi)
    excess_kurtosis_rsdl <- moments::kurtosis(training_data$resi)

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

    out$normality_metrics <- normality_metrics

    out$model <- linregress

    energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 1))
    names(energy_use_summary) <- c("Baseline Energy Use")
    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T),
                                                                 scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
    out$energy_use_summary <- energy_use_summary

    if (!is.null(prediction_data)) {
      predicted_data <- prediction_data %>%
        mutate(pred_eload_perday = prediction_data[['HDDperday']] * m_HDD + prediction_data[['CDDperday']] * m_CDD + intercept) %>%
        mutate(pred_eload = pred_eload_perday * days)

      predicted_data <- predicted_data[complete.cases(predicted_data), ]
      out$post_implementation_data <- predicted_data

      energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 3))
      names(energy_use_summary) <- c("Baseline Energy Use", "Adjusted Baseline Energy Use", "Post Implementation Energy Use")

      energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T),
                                                                   scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Adjusted Baseline Energy Use' <- paste(format(sum(predicted_data$pred_eload, na.rm = T),
                                                                            scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Post Implementation Energy Use' <- paste(format(sum(prediction_data$eload, na.rm = T),
                                                                              scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

      out$energy_use_summary <- energy_use_summary
    }

  } else if (data_interval == "Daily") {

    linregress <- lm(training_data[['eload']] ~ training_data[['HDD']] + training_data[['CDD']])

    intercept <- linregress[[1]][[1]]

    m_HDD <- linregress[[1]][[2]]

    if (is.na(m_HDD)) {
      m_HDD <- 0
    }

    m_CDD <- linregress[[1]][[3]]

    if (is.na(m_CDD)) {
      m_CDD <- 0
    }

    r_value <- summary(linregress)$r.squared

    training_data <- training_data %>%
      mutate(yfit = training_data[['HDD']] * m_HDD + training_data[['CDD']] * m_CDD + intercept) %>%
      mutate(resi = eload - yfit) %>%
      mutate(resi_sq = resi ^ 2)

    training_data <- training_data[complete.cases(training_data), ]

    nparameter <- nrow(as_data_frame(linregress$coefficients))

    MBE <- sum(training_data[['resi']]) / nrow(training_data)
    NDBE <- 100 * (sum(training_data[['resi']] / sum(training_data[['eload']])))
    CVRMSE <- ((sqrt(sum(training_data[['resi_sq']]) / (nrow(training_data) - nparameter))) / mean(training_data[['eload']]))
    CVMAE <- (sum(abs(training_data[['resi']])) / (nrow(training_data) - nparameter)) / mean(training_data[['eload']])

    goodness_of_fit <- data.frame()
    goodness_of_fit[1, 1] <- r_value
    goodness_of_fit[1, 2] <- CVRMSE
    goodness_of_fit[1, 3] <- paste(formatC(NDBE, format = "e", digits = 2), "%", sep = "")
    goodness_of_fit[1, 4] <- nparameter

    colnames(goodness_of_fit) <- c("fit_R2", "fit_CVRMSE", "fit_NDBE", "#Parameters")

    parameters <- as.data.frame(rownames(summary(linregress)$coefficients))
    coefficients <- as.data.frame(summary(linregress)$coefficients[, 1])
    p_values <- as.data.frame(summary(linregress)$coefficients[, 4])

    HDD_CDD_model <- bind_cols(parameters, coefficients, p_values)
    names(HDD_CDD_model) <- c("Parameters", "Coefficients", "p-values")

    out <- NULL
    out$goodness_of_fit <- goodness_of_fit
    out$training_data <- training_data
    out$HDD_CDD_model <- HDD_CDD_model

    skewness_rsdl <- moments::skewness(training_data$resi)
    excess_kurtosis_rsdl <- moments::kurtosis(training_data$resi)

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
    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

    out$energy_use_summary <- energy_use_summary
    out$model <- linregress

    if (!is.null(prediction_data)) {

      predicted_data <- prediction_data %>%
        mutate(pred_eload = prediction_data[['HDD']] * m_HDD + prediction_data[['CDD']] * m_CDD + intercept)

      predicted_data <- predicted_data[complete.cases(predicted_data), ]
      out$post_implementation_data <- predicted_data

      energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 3))
      names(energy_use_summary) <- c("Baseline Energy Use", "Adjusted Baseline Energy Use", "Post Implementation Energy Use")

      energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Adjusted Baseline Energy Use' <- paste(format(sum(predicted_data$pred_eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Post Implementation Energy Use' <- paste(format(sum(prediction_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

      out$energy_use_summary <- energy_use_summary
    }
  }
    return(out)

  }


  if (regression_type == "HDD Regression") {

    data_time_interval <- difftime(training_data$time[2], training_data$time[1], units = "min")

    linregress <- lm(training_data[['eload']] ~ training_data[['HDD']])

    intercept <- linregress[[1]][[1]]

    m_HDD <- linregress[[1]][[2]]

    r_value <- summary(linregress)$r.squared

    training_data <- training_data %>%
      mutate(yfit = training_data[['HDD']] * m_HDD + intercept) %>%
      mutate(resi = eload - yfit) %>%
      mutate(resi_sq = resi ^ 2)

    training_data <- training_data[complete.cases(training_data), ]

    nparameter <- nrow(as_data_frame(linregress$coefficients))

    MBE <- sum(training_data[['resi']]) / nrow(training_data)
    NDBE <- 100 * (sum(training_data[['resi']] / sum(training_data[['eload']])))
    CVRMSE <- ((sqrt(sum(training_data[['resi_sq']]) / (nrow(training_data) - nparameter))) / mean(training_data[['eload']]))
    CVMAE <- (sum(abs(training_data[['resi']])) / (nrow(training_data) - nparameter)) / mean(training_data[['eload']])

    goodness_of_fit <- data.frame()
    goodness_of_fit[1, 1] <- r_value
    goodness_of_fit[1, 2] <- CVRMSE
    goodness_of_fit[1, 3] <- paste(formatC(NDBE, format = "e", digits = 2), "%", sep = "")
    goodness_of_fit[1, 4] <- nparameter

    colnames(goodness_of_fit) <- c("fit_R2", "fit_CVRMSE", "fit_NDBE", "#Parameters")

    parameters <- as.data.frame(rownames(summary(linregress)$coefficients))
    coefficients <- as.data.frame(summary(linregress)$coefficients[, 1])
    p_values <- as.data.frame(summary(linregress)$coefficients[, 4])

    HDD_model <- bind_cols(parameters, coefficients, p_values)
    names(HDD_model) <- c("Parameters", "Coefficients", "p-values")

    out <- NULL
    out$goodness_of_fit <- goodness_of_fit
    out$training_data <- training_data
    out$HDD_model <- HDD_model

    skewness_rsdl <- moments::skewness(training_data$resi)
    excess_kurtosis_rsdl <- moments::kurtosis(training_data$resi)

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
    names(normality_metrics) <- c("Skewness Value", "Skewness Interpretation", "Excess Kurtosis Value", "Excess Kurtosis Interpretation")
    normality_metrics$'Skewness Value' <-  formatC(skewness_rsdl, big.mark = ",")
    normality_metrics$'Skewness Interpretation' <- skewness_rsdl_meaning
    normality_metrics$'Excess Kurtosis Value' <-  formatC(excess_kurtosis_rsdl, big.mark = ",")
    normality_metrics$'Excess Kurtosis Interpretation' <- excess_kurtosis_rsdl_meaning

    out$normality_metrics <- normality_metrics

    energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 1))
    names(energy_use_summary) <- c("Baseline Energy Use")
    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

    out$energy_use_summary <- energy_use_summary

    out$model <- linregress

    if (!is.null(prediction_data)) {

      predicted_data <- prediction_data %>%
        mutate(pred_eload = prediction_data[['HDD']] * m_HDD + intercept)

      predicted_data <- predicted_data[complete.cases(predicted_data), ]
      out$post_implementation_data <- predicted_data

      energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 3))
      names(energy_use_summary) <- c("Baseline Energy Use", "Adjusted Baseline Energy Use", "Post Implementation Energy Use")

      energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Adjusted Baseline Energy Use' <- paste(format(sum(predicted_data$pred_eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
      energy_use_summary$'Post Implementation Energy Use' <- paste(format(sum(prediction_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

      out$energy_use_summary <- energy_use_summary

    }

    return(out)
  }

  if (regression_type == "CDD Regression") {

    data_time_interval <- difftime(training_data$time[2], training_data$time[1], units = "min")

    linregress <- lm(training_data[['eload']] ~ training_data[['CDD']])

    intercept <- linregress[[1]][[1]]

    m_CDD <- linregress[[1]][[2]]

    r_value <- summary(linregress)$r.squared

    training_data <- training_data %>%
      mutate(yfit = training_data[['CDD']] * m_CDD + intercept) %>%
      mutate(resi = eload - yfit) %>%
      mutate(resi_sq = resi ^ 2)

    training_data <- training_data[complete.cases(training_data), ]

    nparameter <- nrow(as_data_frame(linregress$coefficients))

    MBE <- sum(training_data[['resi']]) / nrow(training_data)
    NDBE <- 100 * (sum(training_data[['resi']] / sum(training_data[['eload']])))
    CVRMSE <- ((sqrt(sum(training_data[['resi_sq']]) / (nrow(training_data) - nparameter))) / mean(training_data[['eload']]))
    CVMAE <- (sum(abs(training_data[['resi']])) / (nrow(training_data) - nparameter)) / mean(training_data[['eload']])

    goodness_of_fit <- data.frame()
    goodness_of_fit[1, 1] <- r_value
    goodness_of_fit[1, 2] <- CVRMSE
    goodness_of_fit[1, 3] <- paste(formatC(NDBE, format = "e", digits = 2), "%", sep = "")
    goodness_of_fit[1, 4] <- nparameter

    colnames(goodness_of_fit) <- c("fit_R2", "fit_CVRMSE", "fit_NDBE", "#Parameters")

    parameters <- as.data.frame(rownames(summary(linregress)$coefficients))
    coefficients <- as.data.frame(summary(linregress)$coefficients[, 1])
    p_values <- as.data.frame(summary(linregress)$coefficients[, 4])

    CDD_model <- bind_cols(parameters, coefficients, p_values)
    names(CDD_model) <- c("Parameters", "Coefficients", "p-values")

    out <- NULL
    out$goodness_of_fit <- goodness_of_fit
    out$training_data <- training_data
    out$CDD_model <- CDD_model

    skewness_rsdl <- moments::skewness(training_data$resi)
    excess_kurtosis_rsdl <- moments::kurtosis(training_data$resi)

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

    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

  }

  out$energy_use_summary <- energy_use_summary

  out$model <- linregress

  if (!is.null(prediction_data)) {
    predicted_data <- prediction_data %>%
      mutate(pred_eload = prediction_data[['CDD']] * m_CDD + intercept)

    predicted_data <- predicted_data[complete.cases(predicted_data), ]
    out$post_implementation_data <- predicted_data

    energy_use_summary <- as.data.frame(matrix(nr = 1, nc = 3))
    names(energy_use_summary) <- c("Baseline Energy Use", "Adjusted Baseline Energy Use", "Post Implementation Energy Use")

    energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
    energy_use_summary$'Adjusted Baseline Energy Use' <- paste(format(sum(predicted_data$pred_eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
    energy_use_summary$'Post Implementation Energy Use' <- paste(format(sum(prediction_data$eload, na.rm = T), scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))

  }

  out$energy_use_summary <- energy_use_summary
  return(out)
}
