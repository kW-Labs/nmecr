#' Calculate model summary statistics. Applicable to training data only.
#'
#' @param modeled_data_obj  List with model results. Output from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT.
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with five model statistics: "R_squared", "CVRMSE", "NDBE", "MBE", "#Parameters"
#'
#' @export
#'
calculate_summary_statistics <- function(modeled_data_obj = NULL) {

  # residuals' calculation based on day normalization

  if (modeled_data_obj$model_input_options$day_normalized == TRUE &
             modeled_data_obj$model_input_options$chosen_modeling_interval == "Monthly") {

    model_fit <- modeled_data_obj$training_data$model_fit/modeled_data_obj$training_data$days
    eload <- modeled_data_obj$training_data$eload_perday
    fit_residuals_numeric <- eload - model_fit
    avg.eload_deviation = eload - mean(eload, na.rm = T)

  } else {
    model_fit <- modeled_data_obj$training_data$model_fit
    eload <- modeled_data_obj$training_data$eload
    fit_residuals_numeric <- eload - model_fit
    avg.eload_deviation = eload - mean(eload, na.rm = T)
  }

  # Calculation of model parameter count

  if(modeled_data_obj$model_input_options$regression_type == "TOWT" | modeled_data_obj$model_input_options$regression_type == "TOW") {

    nparameter <- sum(! names(modeled_data_obj$model_occupied$coefficients) %in% "(Intercept)" & !is.na(modeled_data_obj$model_occupied$coefficients))

    intercept_count <-  1

    if(exists("model_unoccupied", where = modeled_data_obj)) {
      nparameter <- nparameter + sum(! names(modeled_data_obj$model_unoccupied$coefficients) %in% "(Intercept)" & !is.na(modeled_data_obj$model_unoccupied$coefficients))
      intercept_count <- intercept_count + 1
    }

  } else {
    nparameter <- sum(!names(modeled_data_obj$model$coefficients) %in% "(Intercept)" & !is.na(modeled_data_obj$model$coefficients))
    intercept_count <-  1
  }

  effective_parameters <- length(fit_residuals_numeric) %>%
    magrittr::subtract(nparameter)

  dof <- length(fit_residuals_numeric) %>%
    magrittr::subtract(nparameter) %>%
    magrittr::subtract(intercept_count)

  # R-sqaured (Absolute)

  R_squared <- 1 - (sum(fit_residuals_numeric^2)/sum(avg.eload_deviation^2))

  # Adjusted R-sqaured (Absolute)

  N <- length(fit_residuals_numeric)

  Adjusted_R_squared <- round(1 - (((1-R_squared)*(N-1))/(dof)),2)

  # Root Mean Sqaured Error (Absolute)
  RMSE <- fit_residuals_numeric %>%
    magrittr::raise_to_power(2) %>%
    sum(na.rm = T) %>%
    magrittr::divide_by(effective_parameters) %>%
    magrittr::raise_to_power(0.5)


  # Coefficient of Variation of the Root Mean Square Error (Percentage)
  CVRMSE <- RMSE %>%
    magrittr::divide_by(mean(eload, na.rm = T)) %>%
    magrittr::multiply_by(100) %>%
    round(., 2)

  # Normalized Mean Bias Error (Percentage)
  NMBE <- fit_residuals_numeric %>%
    sum(na.rm = T) %>%
    magrittr::divide_by(effective_parameters*mean(eload, na.rm = T)) %>%
    magrittr::multiply_by(100) %>%
    format(round(., 2), nsmall = 4)

  # Coefficient of Variation of Mean Absolute error (Absolute)
  CVMAE <- fit_residuals_numeric %>%
    abs(.) %>%
    magrittr::divide_by(length(fit_residuals_numeric)) %>%
    sum(na.rm = T) %>%
    magrittr::divide_by(mean(eload, na.rm = T))

  # Net Determination Bias Error (Percentage)
  NDBE <- fit_residuals_numeric %>%
    sum(na.rm = T) %>%
    magrittr::divide_by(sum(eload, na.rm = T)) %>%
    magrittr::multiply_by(100) %>%
    format(round(., 2), nsmall = 4)

  goodness_of_fit <- as.data.frame(matrix(nrow = 1, ncol = 7))
  names(goodness_of_fit) <- c("R_squared", "Adjusted_R_squared", "CVRMSE %", "NDBE %", "NMBE %", "#Parameters", "deg_of_freedom")
  goodness_of_fit$R_squared <- R_squared
  goodness_of_fit$Adjusted_R_squared <- Adjusted_R_squared
  goodness_of_fit$`CVRMSE %` <- CVRMSE
  goodness_of_fit$`NDBE %` <- NDBE
  goodness_of_fit$`NMBE %` <- NMBE
  goodness_of_fit$"#Parameters" <- nparameter
  goodness_of_fit$"deg_of_freedom" <- dof

  return(goodness_of_fit)

}

