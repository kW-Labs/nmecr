

# Expectations -----

all_stats <- readRDS("Expectations/nmecr_normalized_all_stats.rds")
TOWT_baseline_savings_summary <- readRDS("Expectations/nmecr_normalized_baseline_savings_summ.rds")
normalized_savings_df <- readRDS("Expectations/nmecr_normalized_norm_savings_summ.rds")

# Data and Dataframes -----

data(eload)
data(temp)
data(TMY3)

baseline_df <- nmecr::create_dataframe(eload_data = eload, temp_data = temp,
                                start_date = "03/01/2012 00:00",
                                end_date = "02/28/2013 23:59",
                                convert_to_data_interval = "Daily")


performance_df <- nmecr::create_dataframe(eload_data = eload, temp_data = temp,
                                   start_date = "03/01/2014 00:00",
                                   end_date = "02/28/2015 23:59",
                                   convert_to_data_interval = "Daily")

# Models ----

TOWT_baseline_model <- nmecr::model_with_TOWT(training_data = baseline_df,
                              model_input_options =
                                assign_model_inputs(regression_type = "TOWT"))

TOWT_performance_model <- nmecr::model_with_TOWT(training_data = performance_df,
                                          model_input_options =
                                            nmecr::assign_model_inputs(regression_type = "TOWT"))

# Stats ------

TOWT_baseline_stats <- nmecr::calculate_summary_statistics(TOWT_baseline_model)

TOWT_performance_stats <- nmecr::calculate_summary_statistics(TOWT_performance_model)

all_stats_created <- dplyr::bind_rows(TOWT_baseline_stats, TOWT_performance_stats)

model_names <- c("TOWT Baseline", "TOWT Performance")

all_stats_created <- dplyr::bind_cols("Model Name" = model_names, all_stats_created)

# Savings Summary -----

TOWT_baseline_savings_10 <- nmecr::calculate_savings_and_uncertainty(prediction_df = NULL,
                                                              savings_fraction = 0.1,
                                                              modeled_object = TOWT_baseline_model,
                                                              model_summary_statistics = TOWT_baseline_stats,
                                                              confidence_level = 90)


TOWT_baseline_savings_10_created <- TOWT_baseline_savings_10$savings_summary_df

# Normalized Savings ----

TMY3_daily <- nmecr::aggregate(eload_data = NULL, temp_data = TMY3,
                        convert_to_data_interval = "Daily", temp_balancepoint = 65)

norm_savings <- nmecr::calculate_norm_savings_and_uncertainty(baseline_model = TOWT_baseline_model,
                                                       baseline_stats = TOWT_baseline_stats,
                                                       performance_model = TOWT_performance_model,
                                                       performance_stats = TOWT_performance_stats,
                                                       normalized_weather = TMY3_daily,
                                                       confidence_level = 90)

normalized_savings_df_created <- as.data.frame(norm_savings$normalized_savings_df)

# Tests ---
testthat::test_that('nmecr_normalized_savings vig is consistent', {

  testthat::expect_identical(all_stats, all_stats_created)
  testthat::expect_identical(TOWT_baseline_savings_summary, TOWT_baseline_savings_10_created)
  testthat::expect_identical(normalized_savings_df, normalized_savings_df_created)


})
