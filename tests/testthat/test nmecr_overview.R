# Expectations -----

actual_savings_summary <- readRDS("Expectations/nmecr_overview_savings_summary.rds")
actual_savings_df <- readRDS("Expectations/nmecr_overview_savings_df.rds")

## Note: every df is not tested since a lot of material is similar to nmecr_predictability vignette

# Data and Dataframes -----

data(eload)
data(temp)

baseline_df <- nmecr::create_dataframe(eload_data = eload, temp_data = temp,
                                start_date = "03/01/2012 00:00",
                                end_date = "02/28/2013 23:59",
                                convert_to_data_interval = "Daily")

performance_df <- nmecr::create_dataframe(eload_data = eload, temp_data = temp,
                                   start_date = "03/01/2014 00:00",
                                   end_date = "02/28/2015 23:59",
                                   convert_to_data_interval = "Daily")

# Models ----

TOWT_model <- nmecr::model_with_TOWT(training_data = baseline_df,
                              model_input_options =
                                nmecr::assign_model_inputs(regression_type = "TOWT"))


# Stats ------


TOWT_stats <- nmecr::calculate_summary_statistics(TOWT_model)


# Predictions ----

TOWT_predictions <- nmecr::calculate_model_predictions(training_data = baseline_df,
                                                prediction_data = performance_df,
                                                modeled_object = TOWT_model)

TOWT_savings <- nmecr::calculate_savings_and_uncertainty(prediction_df = TOWT_predictions,
                                                  modeled_object = TOWT_model,
                                                  model_summary_statistics = TOWT_stats,
                                                  confidence_level = 90)

actual_savings_summary_created <- TOWT_savings$savings_summary_df
actual_savings_df_created <- TOWT_savings$savings_df

# Tests ---
testthat::test_that('nmecr_overview vig is consistent', {

  testthat::expect_identical(actual_savings_summary, actual_savings_summary_created)
  testthat::expect_identical(actual_savings_df, actual_savings_df_created)


})
