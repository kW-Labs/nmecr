

# Expectations -----

all_stats <- readRDS("Expectations/nmecr_predictability_all_stats.rds")
TOWT_savings_summary <- readRDS("Expectations/nmecr_predictability_TOWT_savings_summary.rds")

# Data and Dataframes -----

data(eload)
data(temp)

baseline_df <- nmecr::create_dataframe(eload_data = eload, temp_data = temp,
                                start_date = "03/01/2012 00:00",
                                end_date = "02/28/2013 23:59",
                                convert_to_data_interval = "Daily")

# Models ----

SLR_model <- nmecr::model_with_SLR(training_data = baseline_df,
                            model_input_options =
                              assign_model_inputs(regression_type = "SLR"))

Four_P_model <- nmecr::model_with_CP(training_data = baseline_df,
                              model_input_options =
                                assign_model_inputs(regression_type =
                                                      "Four Parameter Linear Model"))

TOWT_model <- nmecr::model_with_TOWT(training_data = baseline_df,
                              model_input_options =
                                assign_model_inputs(regression_type = "TOWT"))


# Stats ------

SLR_stats <- nmecr::calculate_summary_statistics(SLR_model)

Four_P_stats <- nmecr::calculate_summary_statistics(Four_P_model)

TOWT_stats <- nmecr::calculate_summary_statistics(TOWT_model)

all_stats_created <- dplyr::bind_rows(SLR_stats, Four_P_stats, TOWT_stats)

model_names <- c("SLR", "Four Parameter", "TOWT")

all_stats_created <- dplyr::bind_cols("Model Name" = model_names, all_stats_created)

# Savings Summary -----

TOWT_savings_10 <- nmecr::calculate_savings_and_uncertainty(prediction_df = NULL,
                                                     savings_fraction = 0.1,
                                                     modeled_object = TOWT_model,
                                                     model_summary_statistics = TOWT_stats,
                                                     confidence_level = 90)

TOWT_savings_summary_created <- TOWT_savings_10$savings_summary_df

# Tests ---
testthat::test_that('nmecr_predictability vig is consistent', {

  testthat::expect_identical(all_stats, all_stats_created)
  testthat::expect_identical(TOWT_savings_summary, TOWT_savings_summary_created)


})
