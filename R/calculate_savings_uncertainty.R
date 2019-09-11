#' Compute Savings Uncertainity.
#'
#' \code{This function calculates the savings uncertainty, given the savings percent and a confidence level, as per ASHRAE Guideline 14.}
#'
#' @param data The output object from 'compute_avoided_energy_use'.
#' @param savings_percent a numeric indicating the observed/expected savings percentage
#' @return  a savings list object with savings uncertainty and minimum savings needed at 50\% uncertainty for 68\%, 90\%, 95\%, and 99.7\% confidence level.
#'
#' @export

# Savings Analysis ----

calculate_savings_uncertainty <- function(data, savings_percent) {

  if (is.character(data$baseline_data$time)) {
    data$baseline_data$time <- lubridate::mdy_hm(data$baseline_data$time)
  }

  nterval <- difftime(data$baseline_data$time[2],
                      data$baseline_data$time[1], units = "min")

  if (nterval < 60 && nterval > 0 ) {
    data_interval <- "15-min"
  } else if (nterval == 60) {
    data_interval <- "Hourly"
  } else if (nterval == 1440) {
    data_interval <- "Daily"
  } else if (nterval > 2880) {
    data_interval <- "Monthly"
  } else {
    "Check the Data Interval. Reprocess the data to 15-min data, hourly data, or daily data"
  }

  if (data_interval == "Hourly") {
    alpha <- 1.26
  } else if (data_interval == "Daily") {
    d_M <- (nrow(data$baseline_data)) / mean(30, 31)
    alpha <- ( - 0.00024 * (d_M ^ 2) + (0.03535 * (d_M) + 1.00286))
  } else {
    observation_count <- nrow(data$baseline_data)
    alpha <- (- 0.00022 * (observation_count ^ 2)) + (0.03306 * (observation_count)) + 0.94054
  }

  savings_percent <- savings_percent / 100 # approximate percentage of the baseline energy use that is saved.
  u2 <- 0.5 # 50% Uncertainty level

  t_68 <- qt((1 - (1 - (68 / 100)) / 2), 100000)
  t_90 <- qt((1 - (1 - (90 / 100)) / 2), 100000)
  t_95 <- qt((1 - (1 - (95 / 100)) / 2), 100000)
  t_99.7 <- qt((1 - (1 - (99.7 / 100)) / 2), 100000)


  if (data_interval == "Hourly" | data_interval == "Daily") {

    fit_residuals <- as.data.frame(data$baseline_data$eload - data$baseline_data$yfit)
    fit_residuals_shifted <- fit_residuals[- 1, ]
    fit_residuals_all_except_last <- fit_residuals[1 : nrow(fit_residuals) - 1, ]

    rho <- cor(fit_residuals_all_except_last, fit_residuals_shifted)
    rho2 <- (1 - rho) / (1 + rho) # coefficient of determination
    n <- nrow(fit_residuals)
    n2 <- n * rho2

    fit_cvrmse <- data$goodness_of_fit$fit_CVRMSE

    u_68 <- t_68 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) *  (1 / nrow(fit_residuals)))) / savings_percent
    u_90 <- t_90 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) *  (1 / nrow(fit_residuals)))) / savings_percent
    u_95 <- t_95 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) *  (1 / nrow(fit_residuals)))) / savings_percent
    u_99.7 <- t_99.7 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) *  (1 / nrow(fit_residuals)))) / savings_percent

  } else {

    fit_residuals <- as.data.frame(data$baseline_data$eload - data$baseline_data$yfit)
    fit_residuals_shifted <- fit_residuals[- 1, ]
    fit_residuals_all_except_last <- fit_residuals[1 : nrow(fit_residuals) - 1, ]

    rho <- cor(fit_residuals_all_except_last, fit_residuals_shifted)
    rho2 <- (1 - rho) / (1 + rho) # coefficient of determination
    n <- nrow(fit_residuals)
    n2 <- n * rho2

    fit_cvrmse <- data$goodness_of_fit$fit_CVRMSE

    u_68 <- t_68 * alpha * fit_cvrmse * sqrt((1 + (2 / n)) *  (1 / nrow(fit_residuals))) / savings_percent
    u_90 <- t_90 * alpha * fit_cvrmse * sqrt((1 + (2 / n)) *  (1 / nrow(fit_residuals))) / savings_percent
    u_95 <- t_95 * alpha * fit_cvrmse * sqrt((1 + (2 / n)) *  (1 / nrow(fit_residuals))) / savings_percent
    u_99.7 <- t_99.7 * alpha * fit_cvrmse * sqrt((1 + (2 / n)) *  (1 / nrow(fit_residuals))) / savings_percent
  }

  # Savings

  if (data_interval == "Hourly" | data_interval == "Daily") {

    energy_saving_equiv_pct_savings <- savings_percent * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_68 <- t_68 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) * (1 / nrow(fit_residuals)))) / u2
    energy_for_savings_at_50pct_u_68 <- (pct_savings_for_50pct_u_68) * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_90 <- t_90 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) * (1 / nrow(fit_residuals)))) / u2
    energy_for_savings_at_50pct_u_90 <- (pct_savings_for_50pct_u_90) * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_95 <- t_95 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) * (1 / nrow(fit_residuals)))) / u2
    energy_for_savings_at_50pct_u_95 <- (pct_savings_for_50pct_u_95) * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_99.7 <- t_99.7 * alpha * fit_cvrmse * sqrt(((n / n2) * (1 + (2 / n2)) * (1 / nrow(fit_residuals)))) / u2
    energy_for_savings_at_50pct_u_99.7 <- (pct_savings_for_50pct_u_99.7) * sum(data$baseline_data$eload)

  } else {
    energy_saving_equiv_pct_savings <- savings_percent * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_68 <- t_68 * alpha * fit_cvrmse * sqrt((1 + 2 / nrow(data$baseline_data)) * (1 / nrow(data$baseline_data))) / u2
    energy_for_savings_at_50pct_u_68 <- (pct_savings_for_50pct_u_68) * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_90 <- t_90 * alpha * fit_cvrmse * sqrt((1 + 2 / nrow(data$baseline_data)) * (1 / nrow(data$baseline_data))) / u2
    energy_for_savings_at_50pct_u_90 <- (pct_savings_for_50pct_u_90) * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_95 <- t_95 * alpha * fit_cvrmse * sqrt((1 + 2 / nrow(data$baseline_data)) * (1 / nrow(data$baseline_data))) / u2
    energy_for_savings_at_50pct_u_95 <- (pct_savings_for_50pct_u_95) * sum(data$baseline_data$eload)

    pct_savings_for_50pct_u_99.7 <- t_99.7 * alpha * fit_cvrmse * sqrt((1 + 2 / nrow(data$baseline_data)) * (1/nrow(data$baseline_data))) / u2
    energy_for_savings_at_50pct_u_99.7 <- (pct_savings_for_50pct_u_99.7) * sum(data$baseline_data$eload)

  }

  results <- NULL

  uncertainty_summary <- as.data.frame(matrix(nr = 4, nc = 3))
  names(uncertainty_summary) <- c(" ", "Savings Uncertainty", "Minimum Savings needed at 50% uncertainty, as per ASHRAE 14")


  uncertainty_summary[1, 1] <- paste("Uncertainty associated with", savings_percent * 100, "%", "savings at 68% confidence level")
  uncertainty_summary[1, 2] <- paste(round(u_68 * 100, 2), "%", sep = "")
  uncertainty_summary[1, 3] <- format(energy_for_savings_at_50pct_u_68, scientific = FALSE, big.mark = ",", digits = 2)

  uncertainty_summary[2, 1] <- paste("Uncertainty associated with", savings_percent * 100, "%", "savings at 90% confidence level")
  uncertainty_summary[2, 2] <- paste(round(u_90 * 100, 2), "%", sep = "")
  uncertainty_summary[2, 3] <- format(energy_for_savings_at_50pct_u_90, scientific = FALSE, big.mark = ",", digits = 2)

  uncertainty_summary[3, 1] <- paste("Uncertainty associated with", savings_percent * 100, "%", "savings at 95% confidence level")
  uncertainty_summary[3, 2] <- paste(round(u_95 * 100, 2), "%", sep = "")
  uncertainty_summary[3, 3] <- format(energy_for_savings_at_50pct_u_95, scientific = FALSE, big.mark = ",", digits = 2)

  uncertainty_summary[4, 1] <- paste("Uncertainty associated with", savings_percent * 100, "%", "savings at 99.7% confidence level")
  uncertainty_summary[4, 2] <- paste(round(u_99.7 * 100, 2), "%", sep = "")
  uncertainty_summary[4, 3] <- format(energy_for_savings_at_50pct_u_99.7, scientific = FALSE, big.mark = ",", digits = 2)

  results$uncertainty_summary <- uncertainty_summary

  return(results)

}
