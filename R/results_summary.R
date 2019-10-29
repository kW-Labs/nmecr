

# Results' Summary:

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