

# Expectations -----

all_stats <- readRDS("Expectations/nmecr_school_all_stats.rds")
TOWT_savings_summary <- readRDS("Expectations/nmecr_school_savings_summ.rds")

# Data and Dataframes -----

data(school_eload)
data(school_temp)
data(school_op_mode)

baseline_df <- nmecr::create_dataframe(eload_data = school_eload,
                                temp_data = school_temp,
                                convert_to_data_interval = "Daily")

baseline_df_with_op_mode <- nmecr::create_dataframe(eload_data = school_eload,
                                             temp_data = school_temp,
                                             additional_independent_variables = school_op_mode,
                                             additional_variable_aggregation = c(median, median, median, median),
                                             convert_to_data_interval = "Daily")


# Models ----

TOWT_model <- nmecr::model_with_TOWT(training_data = baseline_df,
                              model_input_options =
                                nmecr::assign_model_inputs(regression_type = "TOWT"))

TOWT_model_with_op_mode <- nmecr::model_with_TOWT(training_data = baseline_df_with_op_mode,
                                           model_input_options =
                                             nmecr::assign_model_inputs(regression_type = "TOWT"))


# Stats ------

TOWT_stats <- nmecr::calculate_summary_statistics(TOWT_model)

TOWT_with_op_mode_stats <- nmecr::calculate_summary_statistics(TOWT_model_with_op_mode)


all_stats_created <- dplyr::bind_rows(TOWT_stats, TOWT_with_op_mode_stats)

model_names <- c("TOWT", "TOWT with op_mode")

all_stats_created <- dplyr::bind_cols("Model Name" = model_names, all_stats_created)

# Savings Summary -----

TOWT_savings <- nmecr::calculate_savings_and_uncertainty(prediction_df = NULL,
                                                  savings_fraction = 0.1,
                                                  modeled_object = TOWT_model_with_op_mode,
                                                  model_summary_statistics = TOWT_with_op_mode_stats,
                                                  confidence_level = 90)

TOWT_savings_10_created <- TOWT_savings$savings_summary_df


# Tests ---
testthat::test_that('nmecr_normalized_savings vig is consistent', {

  testthat::expect_identical(all_stats, all_stats_created)
  testthat::expect_identical(TOWT_savings_summary, TOWT_savings_10_created)


})
