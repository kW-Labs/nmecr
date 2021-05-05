context("Model Statistics, Prediction, and Savings Uncertainty")

source("test data types_definitions.R")

test_that("Model summary statistics are being generated", {
  for (i in 1:length(all_models)) {
    expect_s3_class(summ_stats_list[[!!all_models_names[i]]], "data.frame")
  }
})


test_that("Model predictions are being genertated", {
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
  }
})

#Calculate_savings_and_uncertainty Function Testing -----
test_that("All models' savings uncertainty are being generated", {
  for (i in 1:length(all_models)) {
    savs_and_uncert_list[[i]] <- calculate_savings_and_uncertainty(
      prediction_df = all_model_predictions[[i]],
      modeled_object = all_models[[i]],
      model_summary_statistics = summ_stats_list[[i]]
    )

    expect_type(savs_and_uncert_list[[!!all_models_names[i]]], "list")
    expect_s3_class(savs_and_uncert_list[[!!all_models_names[i]]]$savings_df, "data.frame")
    expect_s3_class(savs_and_uncert_list[[!!all_models_names[i]]]$savings_summary_df, "data.frame")
  }
})

test_that("Normalized Savings for SLR is being generated", {
  expect_type(expected_norm_savs_and_uncert, "list")
  expect_s3_class(expected_norm_savs_and_uncert$normalized_savings,
                  "data.frame")
  expect_type(expected_norm_savs_and_uncert$normalized_savings_df, "list")
})

