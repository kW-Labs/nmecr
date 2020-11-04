#' Calculate normalized savings and associated uncertainty as per ASHRAE Guideline 2014
#'
#' \code{This function calculates normalized (hourly or daily) savings achieved and the associated uncertainty.}
#'
#' @param baseline_model Baseline model
#' @param baseline_stats Baseline model's corresponding statistics
#' @param performance_model  Performance model
#' @param performance_stats Performance model's corresponding statistics
#' @param confidence_level Numeric corresponding to the confidence level to be used for savings uncertainty calculation
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
                                                   normalized_weather = NULL, confidence_level = 90){

  if(confidence_level < 0 | confidence_level > 100){
    stop("Error: confidence level cannot be less than zero or greater than 100")
  }

  if(! assertive::is_numeric(confidence_level)){
    stop("Error: confidence level needs to be a numeric input between 0 and 100")
  }

  if (baseline_model$model_input_options$chosen_modeling_interval != "Hourly") {
    normalized_weather <- nmecr::aggregate(temp_data = normalized_weather, convert_to_data_interval = "Daily")
  }

  baseline_normalized = nmecr::calculate_model_predictions(training_data = baseline_model$training_data, prediction_data = normalized_weather,
                                                           modeled_object = baseline_model)

  performance_normalized <- nmecr::calculate_model_predictions(training_data = performance_model$training_data, prediction_data = normalized_weather,
                                                        modeled_object = performance_model)

  normalized_savings <- baseline_normalized

  normalized_savings <- normalized_savings %>%
    rename(norm.baseline = predictions)

  normalized_savings <- normalized_savings %>%
    left_join(performance_normalized, by = c("time", "temp", "HDD", "CDD"))

  normalized_savings <- normalized_savings %>%
    rename(norm.performance = predictions)

  normalized_savings <- normalized_savings %>%
    mutate(norm.savings = norm.baseline - norm.performance)

  # correlation coeff ----

  calculate_independent_points <- function(model_fit_df) {

    corr_df <- as.data.frame(matrix(nrow = length(model_fit_df$time), ncol = 2))
    names(corr_df) <- c("residuals", "residuals_shifted")

    corr_df$residuals <- model_fit_df$eload - model_fit_df$model_fit
    corr_df$residuals_shifted <- dplyr::lag(corr_df$residuals, 1)
    corr_df <- corr_df[-1, ]

    rho <- cor(corr_df[,1], corr_df[,2])
    n <- length(model_fit_df$time)
    n_dash <- n*(1 - rho)/(1+rho)

    return(n_dash)

  }

  baseline_n_dash <- calculate_independent_points(baseline_model$training_data)
  performance_n_dash <- calculate_independent_points(performance_model$training_data)

  baseline_dof <- baseline_stats$deg_of_freedom
  performance_dof <- performance_stats$deg_of_freedom

  baseline_t_stat <- qt(1 - (1 - (confidence_level/100)) / 2, df = baseline_dof)
  performance_t_stat <- qt(1 - (1 - (confidence_level/100)) / 2, df = performance_dof)

  baseline_MSE <- sum((baseline_model$training_data$eload - baseline_model$training_data$model_fit)^2)/baseline_dof
  performance_MSE <- sum((performance_model$training_data$eload - performance_model$training_data$model_fit)^2)/performance_dof

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

  baseline_normalized_uncertainty <- alpha * baseline_t_stat *(mean(baseline_normalized$predictions)/mean(baseline_model$training_data$eload)) *
                                                                 sqrt(baseline_MSE*(1+(2/baseline_n_dash))*365)

  performance_normalized_uncertainty <- alpha * performance_t_stat *(mean(performance_normalized$predictions)/mean(performance_model$training_data$eload)) *
    sqrt(performance_MSE*(1+(2/performance_n_dash))*365)

  normalized_uncertainty <- sqrt(baseline_normalized_uncertainty^2 + performance_normalized_uncertainty^2)

  normalized_uncertainty_pct <- normalized_uncertainty/sum(normalized_savings$norm.baseline - normalized_savings$norm.performance)


  results <- NULL

  results$normalized_savings <- normalized_savings
  results$normalized_savings_frac <- sum(normalized_savings$norm.baseline - normalized_savings$norm.performance)/sum(normalized_savings$norm.baseline)
  results$normalized_savings_unc <- normalized_uncertainty
  results$normalized_savings_unc_pct <- normalized_uncertainty_pct
  results$confidence_level <- confidence_level

  return(results)
}
