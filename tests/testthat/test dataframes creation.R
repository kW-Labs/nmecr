
# Expectations---
simple_df <- readRDS("Expectations/simple_df.rds")
simple_df_hourly <- readRDS("Expectations/simple_df_hourly.rds")
simple_df_daily <- readRDS("Expectations/simple_df_daily.rds")
simple_df_monthly <- readRDS("Expectations/simple_df_monthly.rds")


added_variable_df <- readRDS("Expectations/added_variable_df.rds")
added_variable_df_hourly <- readRDS("Expectations/added_variable_df_hourly.rds")
added_variable_df_daily <- readRDS("Expectations/added_variable_df_daily.rds")
added_variable_df_monthly <- readRDS("Expectations/added_variable_df_monthly.rds")


# Data and Dataframes

eload_data <- readRDS("Processed Data/Processed Eload.rds")
temp_data <- readRDS("Processed Data/Processed Temp.rds")
tmy3_data <- readRDS("Processed Data/Processed TMY3.rds")
additional_variable_data <-  readRDS("Processed Data/Processed Additional Variable.rds")

# Run Functions ----

## Simple dfs ----
simple_df_created <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data)
simple_df_created_hourly <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                    convert_to_data_interval = "Hourly")
simple_df_created_daily <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                   convert_to_data_interval = "Daily")
simple_df_created_monthly <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                     convert_to_data_interval = "Monthly")

## Additional variable dfs -----

added_variable_df_created <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                             additional_independent_variables = additional_variable_data,
                                             additional_variable_aggregation = c(median))


added_variable_df_created_hourly <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                     additional_independent_variables = additional_variable_data,
                                                     additional_variable_aggregation = c(median),
                                                     convert_to_data_interval = "Hourly")

added_variable_df_created_daily <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                   additional_independent_variables = additional_variable_data,
                                                   additional_variable_aggregation = c(median),
                                                   convert_to_data_interval = "Daily")

added_variable_df_created_monthly <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                     additional_independent_variables = additional_variable_data,
                                                     additional_variable_aggregation = c(median),
                                                     convert_to_data_interval = "Monthly")


# Tests ---
testthat::test_that('Simple Dataframes are created correctly', {

  testthat::expect_identical(simple_df, simple_df_created)
  testthat::expect_identical(simple_df_hourly, simple_df_created_hourly)
  testthat::expect_identical(simple_df_daily, simple_df_created_daily)
  testthat::expect_identical(simple_df_monthly, simple_df_created_monthly)

})

testthat::test_that('Dataframes with additional variable are created correctly', {

  testthat::expect_identical(added_variable_df, added_variable_df_created)
  testthat::expect_identical(added_variable_df_hourly, added_variable_df_created_hourly)

  testthat::expect_warning(nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                         additional_independent_variables = additional_variable_data,
                                         additional_variable_aggregation = c(median),
                                         convert_to_data_interval = "Hourly"))

  testthat::expect_identical(added_variable_df_daily, added_variable_df_created_daily)
  testthat::expect_identical(added_variable_df_monthly, added_variable_df_created_monthly)

})

