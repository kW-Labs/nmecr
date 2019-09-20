#' Generate an energy data model using change point models with outside air temperature.
#'
#' \code{This function builds an energy use model using one of four available change point modeling algorithms.}
#'
#' @param training_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param regression_type Character string indictating the modeling algorithm to run: "Three Parameter Heating", "Three Parameter Cooling",
#' "Four Parameter Linear Model", "Five Parameter Linear Model",
#' @param initial_breakpoints vector indicating the initial breakpoints to regress over. Use only with "Five Parameter Linear Model".
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param data_units energy data's units.
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls.}
#'   \item{CP_model}{an object with parameter coefficients and associated p-values resulting from the CP model.}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{model}{the lm object created within 'model_with_CP'.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export

model_with_CP <- function(training_data, prediction_data = NULL,
                                       regression_type = NULL,
                                       initial_breakpoints = NULL,
                                       data_interval = NULL,
                                       data_units = NULL){

  dependent_variable <- training_data$eload
  independent_variable <- training_data$temp

  if (regression_type == "Three Parameter Cooling") {
    dummy_cooling_model <- lm(dependent_variable ~ 1)
    three_paramter_cooling_model <- segmented::segmented(dummy_cooling_model, seg.Z = ~independent_variable)
    yfit <- segmented::predict.segmented(object = three_paramter_cooling_model)

    parameters <- as.data.frame(rownames(summary(three_paramter_cooling_model)$coefficients))
    coefficients <- as.data.frame(summary(three_paramter_cooling_model)$coefficients[, 1])
    p_values <- as.data.frame(summary(three_paramter_cooling_model)$coefficients[, 4])

    CP_model <- dplyr::bind_cols(parameters, coefficients, p_values)
    names(CP_model) <- c("Parameters", "Coefficients", "p-values")

  } else if (regression_type == "Three Parameter Heating") {
    dummy_heating_model <- lm(dependent_variable ~ - 1)
    three_paramter_heating_model <- segmented::segmented(dummy_heating_model, seg.Z = ~independent_variable)
    yfit <- segmented::predict.segmented(object = three_paramter_heating_model)

    parameters <- as.data.frame(rownames(summary(three_paramter_heating_model)$coefficients))
    coefficients <- as.data.frame(summary(three_paramter_heating_model)$coefficients[, 1])
    p_values <- as.data.frame(summary(three_paramter_heating_model)$coefficients[, 4])

    CP_model <- dplyr::bind_cols(parameters, coefficients, p_values)
    names(CP_model) <- c("Parameters", "Coefficients", "p-values")

  } else if (regression_type == "Four Parameter Linear Model"){
    linear_4P_model <- lm(dependent_variable ~ independent_variable)
    four_paramter_linear_model <- segmented::segmented(linear_4P_model, seg.Z = ~independent_variable)
    yfit <- segmented::predict.segmented(object = four_paramter_linear_model)

    parameters <- as.data.frame(rownames(summary(four_paramter_linear_model)$coefficients))
    coefficients <- as.data.frame(summary(four_paramter_linear_model)$coefficients[, 1])
    p_values <- as.data.frame(summary(four_paramter_linear_model)$coefficients[, 4])

    CP_model <- dplyr::bind_cols(parameters, coefficients, p_values)
    names(CP_model) <- c("Parameters", "Coefficients", "p-values")

  } else if (regression_type == "Five Parameter Linear Model") {
    linear_5P_model <- lm(dependent_variable ~ independent_variable)
    validate(
      need(! is.null(min(initial_breakpoints)), ""),
      need(! is.null(max(initial_breakpoints)), ""),
      need(min(initial_breakpoints) > min(independent_variable), "Changepoint 1 is lower than the minimum temperature value available in the data"),
      need(max(initial_breakpoints) < max(independent_variable), "Changepoint 2 is higher than the maximum temperature value available in the data")
    )

   if.false <- F
   while (if.false == F){
     tryCatch ({
       five_paramter_linear_model <- segmented::segmented(linear_5P_model, seg.Z = ~independent_variable, psi = initial_breakpoints)
       if.false <- T
       }, error = function(e){
       }, finally = {})
    }

    yfit <- segmented::predict.segmented(object = five_paramter_linear_model)

    parameters <- as.data.frame(rownames(summary(five_paramter_linear_model)$coefficients))
    coefficients <- as.data.frame(summary(five_paramter_linear_model)$coefficients[, 1])
    p_values <- as.data.frame(summary(five_paramter_linear_model)$coefficients[, 4])

    CP_model <- dplyr::bind_cols(parameters, coefficients, p_values)
    names(CP_model) <- c("Parameters", "Coefficients", "p-values")
  }

  training_data <- training_data %>%
    mutate(yfit = yfit) %>%
    mutate(resi = eload - yfit) %>%
    mutate(resi_sq = resi ^ 2)

  training_data <- training_data[complete.cases(training_data), ]

  resi <-  training_data$eload - yfit
  r_value <- (1 - mean((resi) ^ 2) / var(training_data$eload))

  if (regression_type == "Three Parameter Cooling") {
    nparameter <- nrow(as.data.frame(three_paramter_cooling_model$coefficients))
  } else if (regression_type == "Three Parameter Heating") {
    nparameter <- nrow(as.data.frame(three_paramter_heating_model$coefficients))
  } else if (regression_type == "Four Parameter Linear Model"){
    nparameter <- nrow(as.data.frame(four_paramter_linear_model$coefficients))
  } else if (regression_type == "Five Parameter Linear Model") {
    nparameter <- nrow(as.data.frame(linear_5P_model$coefficients))
  }

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

  out <- NULL
  out$goodness_of_fit <- goodness_of_fit
  out$training_data <- training_data
  out$CP_model <- CP_model

  skewness_rsdl <- moments::skewness(training_data$resi)
  excess_kurtosis_rsdl <- moments::kurtosis(training_data$resi)

  if (skewness_rsdl > - 0.5 && skewness_rsdl < 0.5) {
    skewness_rsdl_meaning <- "Fairly Symmetrical"
  } else if (skewness_rsdl > -1 && skewness_rsdl < -0.5 || skewness_rsdl > 0.5 && skewness_rsdl < 1) {
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
  energy_use_summary$'Baseline Energy Use' <- paste(format(sum(training_data$eload, na.rm = T),
                                                               scientific = FALSE, big.mark = ",", digits = 2), as.character(data_units))
  out$energy_use_summary <- energy_use_summary

  if(regression_type == "Three Parameter Cooling") {
    out$model <-  three_paramter_cooling_model
  } else if (regression_type == "Three Parameter Heating") {
    out$model <-  three_paramter_heating_model
  } else if (regression_type == "Four Parameter Linear Model"){
    out$model <-  linear_4P_model
  } else if (regression_type == "Five Parameter Linear Model") {
    out$model <-  linear_5P_model
  }

  if (!is.null(prediction_data)) {
    new_independent_variable <- data.frame(independent_variable = prediction_data$temp)
    if (regression_type == "Three Parameter Cooling") {
      pred_eload <- segmented::predict.segmented(object = three_paramter_cooling_model, newdata = new_independent_variable)
    } else if (regression_type == "Three Parameter Heating") {
      pred_eload <- segmented::predict.segmented(object = three_paramter_heating_model, newdata = new_independent_variable)
    } else if (regression_type == "Four Parameter Linear Model"){
      pred_eload <- segmented::predict.segmented(object = four_paramter_linear_model, newdata = new_independent_variable)
    } else if (regression_type == "Five Parameter Linear Model") {
      pred_eload <- segmented::predict.segmented(object = five_paramter_linear_model, newdata = new_independent_variable)
    }

    predicted_data <- prediction_data %>%
      mutate(pred_eload =  pred_eload)

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
    return(out)
  }
