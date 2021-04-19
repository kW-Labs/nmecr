#Read in expected data
hourly_start <-
  readxl::read_xlsx("Expected Data/temp_aligned_start_hourly_test.xlsx")
hourly_end <-
  readxl::read_xlsx("Expected Data/temp_aligned_end_hourly_test.xlsx")

minute_start <-
  readxl::read_xlsx("Expected Data/eload_aligned_start_min_test.xlsx")
minute_end <-
  readxl::read_xlsx("Expected Data/eload_aligned_end_min_test.xlsx")

#Read in processed data for testing
eload_file <- "Processed Data/Processed Eload - Electric - kWh.xlsx"
temp_file <- "Processed Data/Processed Temp.xlsx"
eload_data <- readxl::read_xlsx(eload_file)
temp_data <- readxl::read_xlsx(temp_file)

#Create 15 Min interval data frames with align_data
eload_aligned_start_min_test <- align_data(eload_data, "start")
eload_aligned_end_min_test <- align_data(eload_data, "end")

#Create hourly interval data frames with align_data
temp_aligned_start_hourly_test <- align_data(temp_data, "start")
temp_aligned_end_hourly_test <- align_data(temp_data, "end")

#Check col names
expect_equal(colnames(minute_start),
             colnames(eload_aligned_start_min_test))

#Check length
expect_equal(length(hourly_start$time),
              length(temp_aligned_start_hourly_test$time))
expect_equal(length(minute_end$eload),
              length(eload_aligned_end_min_test$eload))

#Compare data frames
#TODO: look into equality of dataframes.
expect_equal(hourly_end, temp_aligned_end_hourly_test)
expect_equal(minute_start, eload_aligned_start_min_test)

#Check col sum
expect_equal(sum(hourly_end$temp),
             sum(temp_aligned_end_hourly_test$temp))
expect_equal(sum(minute_start$eload),
             sum(eload_aligned_start_min_test$eload))

#Check # of NAs
expect_equal(is.na(hourly_end$x),
             is.na(temp_aligned_end_hourly_test$x))
expect_equal(is.na(minute_start$eload),
             is.na(eload_aligned_start_min_test$eload))
