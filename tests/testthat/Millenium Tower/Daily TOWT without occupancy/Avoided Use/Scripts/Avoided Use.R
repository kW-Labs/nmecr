## ---- echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------
require(nmecr)
require(plotly)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Helper Functions.R")


## ---- echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------
# Data Read In

setwd('..')

temp_data <- readxl::read_xlsx(temp_file)
eload_data <- readxl::read_xlsx(eload_file)
if (! is.null(additional_variable_data_file)) {
  additional_data <- readxl::read_xlsx(additional_variable_data_file)
} else {
  additional_data <- NULL
}

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

## Model Stats

baseline_stats <- nmecr::calculate_summary_statistics(baseline_model)

baseline_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.1, modeled_object = baseline_model, 
                                                                 model_summary_statistics = baseline_stats)

# Avoided Use

baseline_predictions <- nmecr::calculate_model_predictions(training_data = baseline_df, prediction_data = performance_df,
                                                           modeled_object = baseline_model)

avoided_use <- nmecr::calculate_savings_and_uncertainty(prediction_df = baseline_predictions, modeled_object = baseline_model,
                                                        model_summary_statistics = baseline_stats, confidence_level = 90)

baseline_results_df <- data.frame(matrix(nrow = 1, ncol = 8))

names(baseline_results_df) <- c('Model', 'Interval', 'R^2', 'Adjusted R^2', 'CV(RMSE) %', 'NMBE %', 'Uncertainty for 10% Savings', 'CL')


baseline_results_df$Model <- modeling_algorithm
baseline_results_df$Interval <- convert_to_data_interval

baseline_results_df[1, c(3:6)] <- baseline_stats[1, c(1:3,5)]
baseline_results_df[1, c(7:8)] <- baseline_uncertainty$savings_summary_df[1, c(2,4)]

baseline_results_df$`NMBE %` <- as.numeric(baseline_results_df$`NMBE %`)
baseline_results_df$`NMBE %` <- round(baseline_results_df$`NMBE %`, 2)
baseline_results_df$`NMBE %` <-  format(baseline_results_df$`NMBE %`, nsmall = 3)

baseline_results_df$`Uncertainty for 10% Savings` <- as.numeric(baseline_results_df$`Uncertainty for 10% Savings`)
baseline_results_df$`Uncertainty for 10% Savings` <- round(baseline_results_df$`Uncertainty for 10% Savings`, 2)
baseline_results_df$`Uncertainty for 10% Savings` <-  format(baseline_results_df$`Uncertainty for 10% Savings`, nsmall = 3)

baseline_results_df <- baseline_results_df %>%
  mutate("Period" = "Baseline") %>%
  select(Period, everything())

avoided_use_df <- avoided_use$savings_summary_df[c(1:5, 7)]
names(avoided_use_df) <- c("Perf. Period Use", "Adjusted Baseline", "Savings", "Savings Fraction", "Savings Uncertainty", "CL")

avoided_use_df$`Expected Avoided Use` <- expected_avoided_use

avoided_use_df <- avoided_use_df %>%
  select(c("Perf. Period Use", "Adjusted Baseline","Expected Avoided Use"), everything())

summary_df <- dplyr::bind_cols(baseline_results_df, as.data.frame(avoided_use$savings_summary_df))

# Write Out Results

result_list <- list("Summary" = summary_df, "Model Fit" = baseline_model$training_data, "Model Predictions" = avoided_use$savings_df)

openxlsx::write.xlsx(result_list, paste0('Results/', project_name, '-Avoided Use.xlsx'))




























## ---- echo = FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------------------
knitr::purl(input = 'Avoided Use.Rmd', output = 'Avoided Use.R')

