## ---- echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------
require(nmecr)
require(plotly)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Helper Functions.R")


## ---- echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------
# Data Read In

setwd('..')

eload_data <- readxl::read_xlsx(eload_file)
temp_data <- readxl::read_xlsx(temp_file)

if (! is.null(additional_variable_data_file)) {
  additional_data <- readxl::read_xlsx(additional_variable_data_file)
} else {
  additional_data <- NULL
}

tmy_data <- readxl::read_xlsx(TMY_temp_file)
year(tmy_data$time) <- year(now())

# Weather Data QC

temp_data_QC_summary <- temp_data %>%
  dplyr::mutate(date = as.Date(time)) %>%
  dplyr::group_by(date) %>%
  summarize(count_of_NAs = sum(is.na(temp))) %>%
  filter(count_of_NAs != 0)

temp_data_QC_summary <- temp_data %>%
  dplyr::mutate(date = as.Date(time)) %>%
  dplyr::group_by(date) %>%
  summarize(count_of_NAs = sum(is.na(temp))) %>%
  filter(count_of_NAs != 0 & count_of_NAs >= 4)

names(temp_data_QC_summary) <- c("", "Hours/Day with Missing Temperature Data")

temp_data <- temp_data %>%
  dplyr::mutate(date = as.Date(time))

temp_data <- within(temp_data, temp[date %in% temp_data_QC_summary$date] <- NA) 

temp_data <- temp_data[, c('time', 'temp')]

# Create Dataframes for Analysis

baseline_df <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data, 
                                       additional_independent_variables = additional_data,
                                       additional_variable_aggregation = additional_variable_aggregation,
                                       convert_to_data_interval = convert_to_data_interval,
                                       start_date = baseline_start_date, end_date = baseline_end_date)


performance_df <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data, 
                                          additional_independent_variables = additional_data,
                                          additional_variable_aggregation = additional_variable_aggregation,
                                          convert_to_data_interval = convert_to_data_interval,
                                          start_date = performance_start_date, end_date = performance_end_date)

# Run Models

model_inputs <- nmecr::assign_model_inputs(timescale_days = NULL, has_temp_knots_defined = FALSE,
                                           equal_temp_segment_points = TRUE, temp_segments_numeric = 6,
                                           temp_knots_value = c(40, 55, 65, 80, 90), initial_breakpoints = c(50, 65), 
                                           regression_type = modeling_algorithm)

baseline_model <- modeling_function(training_data = baseline_df, model_input_options = model_inputs)
performance_model <- modeling_function(training_data = performance_df, model_input_options = model_inputs)


## Model Stats

baseline_stats <- nmecr::calculate_summary_statistics(baseline_model)

baseline_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.1, modeled_object = baseline_model, 
                                                                 model_summary_statistics = baseline_stats)

performance_stats <- nmecr::calculate_summary_statistics(performance_model)

performance_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.1, modeled_object = performance_model, 
                                                                 model_summary_statistics = performance_stats)

# Avoided Use

baseline_predictions <- nmecr::calculate_model_predictions(training_data = baseline_df, prediction_data = performance_df,
                                                           modeled_object = baseline_model)

avoided_use <- nmecr::calculate_savings_and_uncertainty(prediction_df = baseline_predictions, modeled_object = baseline_model,
                                                        model_summary_statistics = baseline_stats, confidence_level = 90)

avoided_use_df <- avoided_use$savings_summary_df[c(1:5, 7)]
names(avoided_use_df) <- c("Perf. Period Use", "Adjusted Baseline", "Savings", "Savings Fraction", "Savings Uncertainty", "CL")

avoided_use_df$`Expected Avoided Use` <- expected_avoided_use

avoided_use_df <- avoided_use_df %>%
  select(c("Perf. Period Use", "Adjusted Baseline","Expected Avoided Use"), everything())

# Normalized Savings

if (convert_to_data_interval != "15-min") {
  tmy_data <- nmecr::aggregate(temp_data = tmy_data, convert_to_data_interval = convert_to_data_interval)
}

if (! is.null(additional_variable_data_file)) { # removing additional variables to normalize to temperature only
  altered_baseline_df <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data, 
                                       convert_to_data_interval = convert_to_data_interval,
                                       start_date = baseline_start_date, end_date = baseline_end_date)
  
  altered_performance_df <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                          convert_to_data_interval = convert_to_data_interval,
                                          start_date = performance_start_date, end_date = performance_end_date)
  
  altered_baseline_model <- modeling_function(training_data = altered_baseline_df, model_input_options = model_inputs)
  altered_performance_model <- modeling_function(training_data = altered_performance_df, model_input_options = model_inputs)
  
  altered_baseline_stats <- nmecr::calculate_summary_statistics(altered_baseline_model)
  altered_performance_stats <- nmecr::calculate_summary_statistics(altered_performance_model)
  
  normalized_savings <- nmecr::calculate_norm_savings_and_uncertainty(baseline_model = altered_baseline_model,
                                                                    baseline_stats = altered_baseline_stats,
                                                                    performance_model = altered_performance_model,
                                                                    performance_stats = altered_performance_stats,
                                                                    normalized_weather = tmy_data,
                                                                    confidence_level = 90)
} else {
  
  normalized_savings <- nmecr::calculate_norm_savings_and_uncertainty(baseline_model = baseline_model,
                                                                    baseline_stats = baseline_stats,
                                                                    performance_model = performance_model,
                                                                    performance_stats = performance_stats,
                                                                    normalized_weather = tmy_data,
                                                                    confidence_level = 90)
  
}

baseline_results_df <- data.frame(matrix(nrow = 1, ncol = 8))
performance_results_df <- data.frame(matrix(nrow = 1, ncol = 6))

names(baseline_results_df) <- c('Model', 'Interval', 'R^2', 'Adjusted R^2', 'CV(RMSE) %', 'NMBE %', 'Uncertainty for 10% Savings', 'CL')
names(performance_results_df) <- c('Model', 'Interval', 'R^2', 'Adjusted R^2', 'CV(RMSE) %', 'NMBE %')

baseline_results_df$Model <- modeling_algorithm
baseline_results_df$Interval <- convert_to_data_interval

performance_results_df$Model <- modeling_algorithm
performance_results_df$Interval <- convert_to_data_interval

baseline_results_df[1, c(3:6)] <- baseline_stats[1, c(1:3,5)]
performance_results_df[1, c(3:6)] <- performance_stats[1, c(1:3,5)]

baseline_results_df[1, c(7:8)] <- baseline_uncertainty$savings_summary_df[1, c(2,4)]

baseline_results_df$`NMBE %` <- as.numeric(baseline_results_df$`NMBE %`)
baseline_results_df$`NMBE %` <- round(baseline_results_df$`NMBE %`, 2)
baseline_results_df$`NMBE %` <-  format(baseline_results_df$`NMBE %`, nsmall = 3)

baseline_results_df$`Uncertainty for 10% Savings` <- as.numeric(baseline_results_df$`Uncertainty for 10% Savings`)
baseline_results_df$`Uncertainty for 10% Savings` <- round(baseline_results_df$`Uncertainty for 10% Savings`, 2)
baseline_results_df$`Uncertainty for 10% Savings` <-  format(baseline_results_df$`Uncertainty for 10% Savings`, nsmall = 3)

performance_results_df$`NMBE %` <- as.numeric(performance_results_df$`NMBE %`)
performance_results_df$`NMBE %` <- round(performance_results_df$`NMBE %`, 2)
performance_results_df$`NMBE %` <-  format(performance_results_df$`NMBE %`, nsmall = 3)

normalized_savings_results <- data.frame(matrix(nrow = 1, ncol = 4))
names(normalized_savings_results) <- c('Norm. Savings', 'Norm. Savings Frac', 'Norm. Savings Uncertainty', 'CL')
normalized_savings_results[1,c(2:4)] <- normalized_savings$normalized_savings_df[c(1,3,4)]
normalized_savings_results[1,1] <- sum(normalized_savings$normalized_savings$norm.savings, na.rm = T)

normalized_savings_results$`Norm. Savings Frac` <- round(normalized_savings_results$`Norm. Savings Frac`, 3)
normalized_savings_results$`Norm. Savings Uncertainty` <- round(normalized_savings_results$`Norm. Savings Uncertainty`, 3)
normalized_savings_results$`Expected Norm. Savings` <- expected_normalized_savings

normalized_savings_results <- normalized_savings_results%>%
  select(c("Expected Norm. Savings", everything()))

summary_df <- dplyr::bind_rows(baseline_results_df, performance_results_df) %>%
  dplyr::bind_rows(avoided_use_df) %>%
  dplyr::bind_rows(normalized_savings_results) %>%
    mutate("Period" = c("Baseline", "Performance", "Avoided Use", "Normalized Savings")) %>%
  select(c(Period, everything()))

# Weather Coverage

baseline_weather_coverage <- nmecr::calculate_coverage(dataframe = baseline_df,
                                                       ref_temp_data = tmy_data,
                                                       outlier_threshold = 1,
                                                       extrapolation_limit = 0.05)

baseline_coverage_summary <- bind_cols(baseline_weather_coverage$coverage_factor_summary,
                                       baseline_weather_coverage$temp_bin_summary)

baseline_coverage_summary <- format_coverage_summary(baseline_coverage_summary) %>%
  mutate("Period" = "Baseline")

baseline_coverage_summary <- pivot_wider(baseline_coverage_summary, names_from = Measure, values_from = Value)

performance_weather_coverage <- nmecr::calculate_coverage(dataframe = performance_df,
                                                   ref_temp_data = tmy_data,
                                                   outlier_threshold = 1,
                                                   extrapolation_limit = 0.05)

performance_coverage_summary <- bind_cols(performance_weather_coverage$coverage_factor_summary,
                                          performance_weather_coverage$temp_bin_summary)

performance_coverage_summary <- format_coverage_summary(performance_coverage_summary) %>%
  mutate("Period" = "Performance")

performance_coverage_summary <- pivot_wider(performance_coverage_summary, names_from = Measure, values_from = Value)


coverage_summary <- dplyr::bind_rows(baseline_coverage_summary, performance_coverage_summary)

coverage_summary <- coverage_summary %>%
  select(Period, everything())


# Write Out Results

result_list <- list("Suumary" = summary_df, "Baseline Model Fit" = baseline_model$training_data, 
                    "Perf Model Fit" = performance_model$training_data,
                    "Avoided Use" = avoided_use$savings_df,
                    "Norm Savings" = normalized_savings$normalized_savings,
                    "Baseline Coverage df" = baseline_weather_coverage$temp_coverage,
                    "Performance Coverage df" = performance_weather_coverage$temp_coverage)

openxlsx::write.xlsx(result_list, paste0('Results/', project_name, '-Normalized Savings.xlsx'))










































## ---- echo = FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------------------
knitr::purl(input = 'Normalized Savings.Rmd', output = 'Normalized Savings.R')

