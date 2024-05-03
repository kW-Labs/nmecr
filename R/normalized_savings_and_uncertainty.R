#' Calculate normalized savings and associated uncertainty as per ASHRAE Guideline 2014
#'
#' \code{This function calculates normalized (hourly or daily) savings achieved and the associated uncertainty.}
#'
#' @param baseline_model Baseline model
#' @param baseline_stats Baseline model's corresponding statistics
#' @param performance_model  Performance model
#' @param performance_stats Performance model's corresponding statistics
#' @param normalized_weather A dataframe with weather data to be used for model prediction normalization
#' @param confidence_level Numeric corresponding to the confidence level to be used for savings uncertainty calculation
#' @param normalized_additional_variables A dataframe of additional variables to be used for model prediction normalization
#' @param additional_variable_aggregation A list of aggregation functions to be used for summarizing additional variables. 
#'
#' @importFrom magrittr %>%
#'
#' @return a list with the following components:
#' \describe{
#'   \item{normalized_savings}{a dataframe with normalized baseline and performance predictions, and savings.}
#'   \item{normalized_savings_frac}{a numeric indicating the fraction of normalized savings against normalized baseline}
#'   \item{normalized_savings_unc}{a numeric indicating normalized savings uncertainty in energy units}
#'   \item{normalized_savings_unc_pct}{a numeric indicating normalized savings uncertainty in percent units}
#'   \item{confidence_leve}{a numeric indicating the confidence level at which the computation was performed.}
#' }
#'
#' @export

calculate_norm_savings_and_uncertainty <- function(baseline_model = NULL, baseline_stats = NULL,
                                                            performance_model = NULL, performance_stats = NULL,
                                                            normalized_weather = NULL, confidence_level = 90,
                                                            normalized_additional_variables = NULL, additional_variable_aggregation = NULL){
  
  predictions <- norm.baseline <- norm.performance <- NULL # No visible binding for global variable
  
  if(confidence_level < 0 | confidence_level > 100){
    stop("Error: confidence level cannot be less than zero or greater than 100")
  }
  
  if(! assertive::is_numeric(confidence_level)){
    stop("Error: confidence level needs to be a numeric input between 0 and 100")
  }
  
  if(baseline_model$model_input_options$chosen_modeling_interval != performance_model$model_input_options$chosen_modeling_interval) {
    stop('Baseline and Performance Period models need to be over the same data interval')
  } else {
    
    if (baseline_model$model_input_options$chosen_modeling_interval != "15-min") {
      normalized_weather <- nmecr::aggregate(temp_data = normalized_weather, convert_to_data_interval = baseline_model$model_input_options$chosen_modeling_interval)
    }
    
  }
  
  # Create the prediction dataframe to be used for model predictions.
  # If additional independent variables are supplied, aggregate them to the same interval as the baseline model and join to the normalized weather
  if (! is.null(normalized_additional_variables)) {
    
    prediction_dataframe <- nmecr::aggregate(temp_data = normalized_weather,
                                             additional_independent_variables = normalized_additional_variables,
                                             additional_variable_aggregation = additional_variable_aggregation,
                                             convert_to_data_interval = baseline_model$model_input_options$chosen_modeling_interval)
    
  } else {
    
    prediction_dataframe <- normalized_weather
    
  }
  
  baseline_normalized <- nmecr::calculate_model_predictions(training_data = baseline_model$training_data, prediction_data = prediction_dataframe,
                                                            modeled_object = baseline_model)
  
  performance_normalized <- nmecr::calculate_model_predictions(training_data = performance_model$training_data, prediction_data = prediction_dataframe,
                                                               modeled_object = performance_model)
  
  normalized_savings <- baseline_normalized
  
  normalized_savings <- normalized_savings %>%
    dplyr::rename(norm.baseline = predictions)
  
  if (baseline_model$model_input_options$chosen_modeling_interval == "Daily") {
    normalized_savings <- normalized_savings %>%
      dplyr::left_join(performance_normalized, by = c("time", "temp", "HDD", "CDD"))
  } else if (baseline_model$model_input_options$chosen_modeling_interval == "Monthly") {
    
    if (baseline_model$model_input_options$day_normalized & performance_model$model_input_options$day_normalized) { # Both models are day-normalized
      normalized_savings <- normalized_savings %>%
        dplyr::left_join(performance_normalized, by = c("time", "temp", "HDD", "CDD", "HDD_perday", "CDD_perday", "days"))
    } else if (! (baseline_model$model_input_options$day_normalized & performance_model$model_input_options$day_normalized) ) { # None of the models are day-normalized
      normalized_savings <- normalized_savings %>%
        dplyr::left_join(performance_normalized, by = c("time", "temp", "HDD", "CDD"))
    } else {
      stop('Both models need to be either day-normalized or not. Cannot process a mix of the two options.')
    }
  } else { # Hourly or 15-min
    normalized_savings <- normalized_savings %>%
      dplyr::left_join(performance_normalized, by = c("time", "temp"))
  }
  
  normalized_savings <- normalized_savings %>%
    dplyr::rename(norm.performance = predictions)
  
  normalized_savings <- normalized_savings %>%
    dplyr::mutate(norm.savings = norm.baseline - norm.performance)
  
  if (baseline_model$model_input_options$chosen_modeling_interval == "Hourly" |
      baseline_model$model_input_options$chosen_modeling_interval == "15-min") {
    
    alpha <- 1.26
    
  } else if (baseline_model$model_input_options$chosen_modeling_interval == "Daily") {
    
    observation_count <- 12
    
    alpha <- ( - 0.00024 * (observation_count ^ 2) + (0.03535 * (observation_count) + 1.00286))
    
  } else if (baseline_model$model_input_options$chosen_modeling_interval == "Monthly") {
    
    observation_count <- 12
    
    alpha <- (- 0.00022 * (observation_count ^ 2)) + (0.03306 * (observation_count)) + 0.94054
  }
  
  # correlation coeff ----
  
  calculate_independent_points <- function(model_fit_df) {
    
    corr_df <- as.data.frame(matrix(nrow = length(model_fit_df$time), ncol = 2))
    names(corr_df) <- c("residuals", "residuals_shifted")
    
    corr_df$residuals <- model_fit_df$eload - model_fit_df$model_fit
    corr_df$residuals_shifted <- dplyr::lag(corr_df$residuals, 1)
    corr_df <- corr_df[-1, ]
    
    rho <- stats::cor(corr_df[,1], corr_df[,2])
    n <- length(model_fit_df$time)
    n_dash <- n*(1 - rho)/(1+rho)
    
    return(n_dash)
    
  }
  
  n <- nrow(baseline_model$training_data)
  m <- nrow(performance_model$training_data)
  g <- nrow(prediction_dataframe)
  
  n_dash <- calculate_independent_points(baseline_model$training_data)
  m_dash <- calculate_independent_points(performance_model$training_data)
  
  baseline_dof <- baseline_stats$deg_of_freedom
  performance_dof <- performance_stats$deg_of_freedom
  baseline_dof_dash <- n_dash - baseline_stats$`#Parameters`
  performance_dof_dash <- m_dash - performance_stats$`#Parameters`
  
  baseline_t_stat <- stats::qt(1 - (1 - (confidence_level/100)) / 2, df = baseline_dof)
  performance_t_stat <- stats::qt(1 - (1 - (confidence_level/100)) / 2, df = performance_dof)
  baseline_t_stat_dash <- stats::qt(1 - (1 - (confidence_level/100)) / 2, df = baseline_dof_dash)
  performance_t_stat_dash <- stats::qt(1 - (1 - (confidence_level/100)) / 2, df = performance_dof_dash)
  
  mean_baseline_eload = mean(baseline_model$training_data$eload, na.rm = T)
  mean_baseline_normalized_eload = mean(baseline_normalized$predictions, na.rm = T)
  mean_performance_eload = mean(performance_model$training_data$eload, na.rm = T)
  mean_performance_normalized_eload = mean(performance_normalized$predictions, na.rm = T)
  
  baseline_squared_error <- (baseline_model$training_data$eload - baseline_model$training_data$model_fit) %>%
    magrittr::raise_to_power(2)
  
  performance_squared_error <- (performance_model$training_data$eload - performance_model$training_data$model_fit) %>%
    magrittr::raise_to_power(2)
  
  baseline_MSE <-  sum(baseline_squared_error, na.rm = T)/baseline_dof
  performance_MSE <- sum(performance_squared_error, na.rm = T)/performance_dof
  baseline_MSE_dash <-  sum(baseline_squared_error, na.rm = T)/baseline_dof_dash
  performance_MSE_dash <- sum(performance_squared_error, na.rm = T)/performance_dof_dash
  
  if (baseline_model$model_input_options$chosen_modeling_interval == "Monthly") {
    
    baseline_normalized_uncertainty <- (baseline_t_stat * alpha)*(mean_baseline_normalized_eload/mean_baseline_eload) * sqrt(baseline_MSE * (1+(2/n))  * g)
    performance_normalized_uncertainty <- (performance_t_stat * alpha)*(mean_performance_normalized_eload/mean_performance_eload) * sqrt(performance_MSE * (1+(2/m))  * g)
    
  } else {
    
    baseline_normalized_uncertainty <- (baseline_t_stat_dash * alpha)*(mean_baseline_normalized_eload/mean_baseline_eload) * sqrt(baseline_MSE_dash * (1+(2/n_dash))  * g)
    performance_normalized_uncertainty <- (performance_t_stat_dash * alpha)*(mean_performance_normalized_eload/mean_performance_eload) * sqrt(performance_MSE_dash * (1+(2/m_dash))  * g)
    
  }
  
  normalized_uncertainty <- sqrt(baseline_normalized_uncertainty^2 + performance_normalized_uncertainty^2)
  
  normalized_uncertainty_frac <- normalized_uncertainty/sum(normalized_savings$norm.baseline - normalized_savings$norm.performance)
  
  
  results <- NULL
  
  results$normalized_savings <- normalized_savings
  
  normalized_savings_df <- list()
  normalized_savings_df$normalized_savings_frac <- sum(normalized_savings$norm.baseline - normalized_savings$norm.performance)/sum(normalized_savings$norm.baseline)
  normalized_savings_df$normalized_savings_unc <- normalized_uncertainty
  normalized_savings_df$normalized_savings_unc_frac <- normalized_uncertainty_frac
  normalized_savings_df$confidence_level <- confidence_level
  
  results$normalized_savings_df <- normalized_savings_df
  
  return(results)
}