context('testing energy and temp dataframes')

# Expectations---
simple_df <- readRDS("Expectations/simple_df.rds")
simple_df_hourly <- readRDS("Expectations/simple_df_hourly.rds")
simple_df_daily <- readRDS("Expectations/simple_df_daily.rds")
simple_df_monthly <- readRDS("Expectations/simple_df_monthly.rds")


# Data and Dataframes

eload_data <- readRDS("Processed Data/Processed Eload.rds")
temp_data <- readRDS("Processed Data/Processed Temp.rds")
tmy3_data <- readRDS("Processed Data/Processed TMY3.rds")
additional_variable_data <-  readRDS("Processed Data/Processed Additional Variable.rds")

# Run Functions ----

simple_df_created <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data)
simple_df_created_hourly <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                    convert_to_data_interval = "Hourly")
simple_df_created_daily <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                   convert_to_data_interval = "Daily")
simple_df_created_monthly <- nmecr::create_dataframe(eload_data = eload_data, temp_data = temp_data,
                                                     convert_to_data_interval = "Monthly")



# Tests ---
testthat::test_that('Simple Dataframes are created correctly', {

  expect_identical(simple_df, simple_df_created)
  expect_identical(simple_df_hourly, simple_df_created_hourly)
  expect_identical(simple_df_daily, simple_df_created_daily)
  expect_identical(simple_df_monthly, simple_df_created_monthly)

})

