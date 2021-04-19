# Source main file
source("test data types_main.R")

#Aggregate Function Testing -----
aggregated_expected <-
  aggregate(
    eload_data = eload_data,
    temp_data = temp_data,
    convert_to_data_interval = "Daily"
  )
expect_s3_class(aggregated_expected, "data.frame")

#Align_data Function Testing -----
eload_aligned_end <- align_data(eload_data, "end")
temp_aligned_start <- align_data(temp_data, "start")

expect_s3_class(eload_aligned_end, "data.frame")
expect_s3_class(temp_aligned_start, "data.frame")

#Assign_model_inputs Function Testing -----
model_inputs_test <-
  assign_model_inputs(regression_type = "SLR")
expect_type(model_inputs_test, "list")
