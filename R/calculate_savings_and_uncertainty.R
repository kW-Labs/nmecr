#' Calculate meter-based savings and associated uncertainty as per ASHARE Guideline 14
#'
#' \code{This function calculates meter-based savings achieved and the associated uncertainty.}
#'
#' @param prediction_df Model predictions. Output from calculate_model_predictions
#' @param savings_fraction savings fraction value to use in savings uncertainty calculations. Used when prediction_df is set to NULL
#' @param modeled_object  List with model results. Output from model_with_SLR, model_with_CP, model_with_HDD_CDD, and model_with_TOWT.
#' @param model_summary_statistics Dataframe with model statistics. Output from calculate_summary_statistics.
#' @param confidence_level Numeric corresponding to the confidence level to be used for savings uncertainty calculation
#'
#' @importFrom magrittr %>%
#'
#' @return a list with the following components:
#' \describe{
#'   \item{savings_df}{a dataframe with savings achieved for each timestamp.}
#'   \item{savings_summary_df}{a dataframe with total performance perioenergy use, total adjusted baseline use, total savingsachieved , savings percentage,
#'   savings uncertainty, savings percentage using 50 percent uncertainty, and the specified confidence level}
#' }
#'
#' @export

calculate_savings_and_uncertainty <- function(prediction_df = NULL, savings_fraction = 0.1, modeled_object = NULL, model_summary_statistics = NULL, confidence_level = 90){

  predictions <- eload <- NULL # No visible binding for global variable

  if(confidence_level < 0 | confidence_level > 100){
    stop("Error: confidence level cannot be less than zero or greater than 100")
  }

  if(! is.numeric(confidence_level)){
    stop("Error: confidence level needs to be a numeric input between 0 and 100")
  }

  if(! is.numeric(savings_fraction)) {
    stop("Error: confidence level needs to be a numeric input between 0 and 1")
  }

  correlation_df <- as.data.frame(matrix(nrow = length(modeled_object$training_data$eload), ncol = 2))
  names(correlation_df) <- c("residuals", "residuals_shifted")

  correlation_df$residuals <- modeled_object$training_data$eload - modeled_object$training_data$model_fit
  correlation_df$residuals_shifted <- dplyr::lag(correlation_df$residuals, 1)
  correlation_df <- correlation_df[-1, ]

  rho <- stats::cor(correlation_df[,1], correlation_df[,2])
  n <- length(modeled_object$training_data$time)
  n_dash <- n*(1 - rho)/(1+rho)

  results <- NULL

  if(! is.null(prediction_df)) {

    savings_df <- prediction_df[, c("time", "eload", "predictions")] %>%
      dplyr::mutate(savings = predictions - eload)

    savings_summary_df <- as.data.frame(matrix(nrow = 1, ncol = 4))
    names(savings_summary_df) <- c("performance_period_use", "adjusted_baseline_use", "savings", "savings_fraction")
    savings_summary_df$performance_period_use <- sum(savings_df$eload, na.rm = T)
    savings_summary_df$adjusted_baseline_use <- sum(savings_df$predictions, na.rm = T)
    savings_summary_df$savings <- sum(savings_df$savings, na.rm = T)
    savings_summary_df$savings_fraction <- round(savings_summary_df$savings/savings_summary_df$adjusted_baseline_use, 2)

    m <- length(prediction_df$time)

    results$savings_df <-  savings_df

  } else {

    m <- n
    savings_summary_df <- as.data.frame(matrix(nrow = 1, ncol = 1))
    names(savings_summary_df) <- "savings_fraction"
    savings_summary_df$savings_fraction <- savings_fraction

  }

  # savings uncertainty ----

  uncertainty_50 <- 0.5

  if (modeled_object$model_input_options$chosen_modeling_interval == "Hourly" |
      modeled_object$model_input_options$chosen_modeling_interval == "15-min") {

    alpha <- 1.26

  } else if (modeled_object$model_input_options$chosen_modeling_interval == "Daily") {

    observation_count <- length(modeled_object$training_data$time) %>%
      magrittr::divide_by(mean(30, 31))

    alpha <- ( - 0.00024 * (observation_count ^ 2) + (0.03535 * (observation_count) + 1.00286))

  } else if (modeled_object$model_input_options$chosen_modeling_interval == "Monthly") {

    observation_count <- length(modeled_object$training_data$time)

    alpha <- (- 0.00022 * (observation_count ^ 2)) + (0.03306 * (observation_count)) + 0.94054
  }

  mean_training_eload = mean(modeled_object$training_data$eload, na.rm = T)
  squared_error <- (modeled_object$training_data$eload - modeled_object$training_data$model_fit) %>%
    magrittr::raise_to_power(2)

  deg_of_freedom = model_summary_statistics$deg_of_freedom
  t_stat = stats::qt(1 - (1 - (confidence_level/100)) / 2, df = deg_of_freedom)
  MSE = sum(squared_error, na.rm = T)/deg_of_freedom

  deg_of_freedom_dash <- n_dash - model_summary_statistics$`#Parameters`
  t_stat_dash <- suppressWarnings(stats::qt(1 - (1 - (confidence_level/100)) / 2, df = deg_of_freedom_dash))
  MSE_dash = sum(squared_error, na.rm = T)/deg_of_freedom_dash

  if (modeled_object$model_input_options$chosen_modeling_interval == "Monthly" | rho < 0.5) { # ASHRAE Guideline14-104, pg 91

    savings_uncertainty <- ((t_stat * alpha)/(m * mean_training_eload * savings_summary_df$savings_fraction)) * sqrt(MSE * (1+(2/n))  * m)
    savings_frac_for_50pct_uncertainty <-  ((t_stat * alpha)/(m * mean_training_eload * uncertainty_50)) * sqrt(MSE * (1+(2/n))  * m)

  } else {

    if (deg_of_freedom_dash < 0) {
      stop("Please review the model coefficients. Negative degrees of freedom found after accouting for autocorrelation.")
    }

    savings_uncertainty <- ((t_stat_dash * alpha)/(m * mean_training_eload * savings_summary_df$savings_fraction)) * sqrt(MSE_dash * (1+(2/n_dash))  * m)
    savings_frac_for_50pct_uncertainty <-  ((t_stat_dash * alpha)/(m * mean_training_eload * uncertainty_50)) * sqrt(MSE_dash * (1+(2/n_dash))  * m)

  }

  savings_summary_df <- savings_summary_df %>%
    dplyr::mutate(savings_uncertainty = savings_uncertainty) %>%
    dplyr::mutate(savings_frac_for_50pct_uncertainty = savings_frac_for_50pct_uncertainty) %>%
    dplyr::mutate(confidence_level = confidence_level)

  results$savings_summary_df <- savings_summary_df

  return(results)
}
