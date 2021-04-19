# #Source Data
# source("test data types_definitions.R")

#Calculate_coverage Function Testing -----
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

#Aggregate Function Testing -----
expect_s3_class(aggregated_expected, "data.frame")

#Align_data Function Testing -----
expect_s3_class(eload_aligned_end, "data.frame")
expect_s3_class(temp_aligned_start, "data.frame")

#Assign_model_inputs Function Testing -----
expect_type(model_inputs_test, "list")
