# Source main file
source("test data types_modeling.R")

#Calculate_summary_statistics Function Testing -----
summ_stats_list <-
  vector(mode = "list", length = length(all_models))
names(summ_stats_list) <- names(all_models)
for (i in 1:length(all_models)) {
  summ_stats_list[[i]] <-
    calculate_summary_statistics(modeled_data_obj = all_models[[i]])

  expect_s3_class(summ_stats_list[[!!all_models_names[i]]], "data.frame")

  if (i == length(all_models)) {
    ## we can consider adding if statements like these to print at the end of for loops, just an idea
    print("iterated through summ stats test")
  }
}
expect_type(summ_stats_list[[!!all_models_names[i]]], "integer")

#Calculate_model_predictions Function Testing -----
all_model_predictions <-
  vector(mode = "list", length = length(all_models))
names(all_model_predictions) <- names(all_models)

for (i in 1:length(all_models)) {
  if (all_models[[i]]$model_input_options$chosen_modeling_interval == "Hourly") {
    training_data_predictions <- hourly_baseline
    prediction_data_predictions <- hourly_performance
  }
  if (all_models[[i]]$model_input_options$chosen_modeling_interval == "Daily") {
    training_data_predictions <- daily_baseline
    prediction_data_predictions <- daily_performance
  }
  if (all_models[[i]]$model_input_options$chosen_modeling_interval == "Monthly") {
    training_data_predictions <- monthly_baseline
    prediction_data_predictions <- monthly_performance
  }
  modeled_object_predictions <- all_models[[i]]
  all_model_predictions[[i]] <-
    calculate_model_predictions(
      training_data = training_data_predictions,
      prediction_data = prediction_data_predictions,
      modeled_object = modeled_object_predictions
    )
  expect_s3_class(all_model_predictions[[!!all_models_names[i]]], "data.frame")
  if (i == length(all_models)) {
    ## we can consider adding if statements like these to print at the end of for loops, just an idea
    print("iterated through predictions test")
  }
}
expect_type(all_model_predictions[[!!all_models_names[i]]], "integer")

#Calculate_savings_and_uncertainty Function Testing -----
savs_and_uncert_list <-
  vector(mode = "list", length = length(all_models))
names(savs_and_uncert_list) <- names(all_models)
for (i in 1:length(all_models)) {
  savs_and_uncert_list[[i]] <- calculate_savings_and_uncertainty(
    prediction_df = all_model_predictions[[i]],
    modeled_object = all_models[[i]],
    model_summary_statistics = summ_stats_list[[i]]
  )

  expect_type(savs_and_uncert_list[[!!all_models_names[i]]], "list")
  expect_s3_class(savs_and_uncert_list[[!!all_models_names[i]]]$savings_df, "data.frame")
  expect_s3_class(savs_and_uncert_list[[!!all_models_names[i]]]$savings_summary_df, "data.frame")
  if (i == length(all_models)) {
    ## we can consider adding if statements like these to print at the end of for loops, just an idea
    print("iterated through savs & uncert test")
  }
}
expect_s3_class(savs_and_uncert_list[[!!all_models_names[i]]], "data.frame")

#Calculate_coverage Function Testing -----
expected_coverage_daily <-
  calculate_coverage(
    dataframe = daily_baseline,
    ref_temp_data = tmy3_data,
    outlier_threshold = 1,
    extrapolation_limit = .05
  )
expected_coverage_hourly <-
  calculate_coverage(
    dataframe = hourly_baseline,
    ref_temp_data = tmy3_data,
    outlier_threshold = 1,
    extrapolation_limit = .05
  )

expect_type(expected_coverage_daily, "list")
expect_s3_class(expected_coverage_daily$temp_coverage, "data.frame")
expect_s3_class(expected_coverage_daily$coverage_factor_summary,
                "data.frame")
expect_s3_class(expected_coverage_daily$temp_bin_summary, "data.frame")

expect_type(expected_coverage_hourly, "list")
expect_s3_class(expected_coverage_hourly$temp_coverage, "data.frame")
expect_s3_class(expected_coverage_hourly$coverage_factor_summary,
                "data.frame")
expect_s3_class(expected_coverage_hourly$temp_bin_summary, "data.frame")

#Calculate_norm_savings_and_uncertainty Function Testing -----
#Create performance model and summ stats for testing
performance_SLR_model_daily <-
  model_with_SLR(training_data = daily_performance, model_input_options = SLR_model_inputs)
performance_stats <-
  calculate_summary_statistics(modeled_data_obj = performance_SLR_model_daily)

expected_norm_savs_and_uncert <-
  calculate_norm_savings_and_uncertainty(
    baseline_model = SLR_model_daily,
    baseline_stats = summ_stats_list[["SLR_model_daily"]],
    performance_model = performance_SLR_model_daily,
    performance_stats = performance_stats,
    normalized_weather = tmy3_data
  )

expect_type(expected_norm_savs_and_uncert, "list")
expect_s3_class(expected_norm_savs_and_uncert$normalized_savings,
                "data.frame")
expect_type(expected_norm_savs_and_uncert$normalized_savings_df, "list")
