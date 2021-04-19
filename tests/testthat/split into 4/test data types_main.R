#Notes
## File tests the output types for each nmecr function (in namespace)
## See notes document: notes v2.docx

#Read in processed data for testing -----
eload_file <- "Processed Data/Processed Eload - Electric - kWh.xlsx"
temp_file <- "Processed Data/Processed Temp.xlsx"
tmy3_file <- "Processed Data/TMY3.xlsx"

eload_data <- readxl::read_xlsx(eload_file)
temp_data <- readxl::read_xlsx(temp_file)
tmy3_data <- readxl::read_xlsx(tmy3_file)

#Inputs -----
baseline_start_date <- "01/01/2019 00:00"
baseline_end_date <- "12/31/2019 23:45"
performance_start_date <- "01/01/2020 00:00"
performance_end_date <- "12/31/2020 23:45"

# Create_Dataframe Function Testing --------
daily_baseline <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "sum",
    start_date = baseline_start_date,
    end_date = baseline_end_date,
    convert_to_data_interval = "Daily",
    temp_balancepoint = 65
  )

daily_performance <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "sum",
    start_date = performance_start_date,
    end_date = performance_end_date,
    convert_to_data_interval = "Daily",
    temp_balancepoint = 65
  )

hourly_baseline <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "sum",
    start_date = baseline_start_date,
    end_date = baseline_end_date,
    convert_to_data_interval = "Hourly",
    temp_balancepoint = 65
  )

hourly_performance <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "sum",
    start_date = performance_start_date,
    end_date = performance_end_date,
    convert_to_data_interval = "Hourly",
    temp_balancepoint = 65
  )

monthly_baseline <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "sum",
    start_date = baseline_start_date,
    end_date = baseline_end_date,
    convert_to_data_interval = "Monthly",
    temp_balancepoint = 65
  )

monthly_performance <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "sum",
    start_date = performance_start_date,
    end_date = performance_end_date,
    convert_to_data_interval = "Monthly",
    temp_balancepoint = 65
  )

dataframe_list <-
  list(
    daily_baseline,
    daily_performance,
    hourly_baseline,
    hourly_performance,
    monthly_baseline,
    monthly_performance
  )
dataframe_names <-
  c(
    "daily_baseline",
    "daily_performance",
    "hourly_baseline",
    "hourly_performance",
    "monthly_baseline",
    "monthly_performance"
  )
names(dataframe_list) <- dataframe_names

for (i in 1:length(dataframe_list)) {
  expect_s3_class(dataframe_list[[!!dataframe_names[i]]], "data.frame")
}
# source("test data types_modeling.R")
# source("test data types_summ stats and uncertainty.R")
# source("test data types_other.R")

# test_file("test data types_modeling.R")
# test_file("test data types_summ stats and uncertainty.R")
# test_file("test data types_other.R")
