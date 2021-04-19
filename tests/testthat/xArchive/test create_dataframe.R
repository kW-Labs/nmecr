# Read in expected data
daily_TOWT_no_occ_expected <-
  readxl::read_xlsx(
    "Millenium Tower/Daily TOWT without occupancy/Normalized Savings/Results/Millennium Tower-Normalized Savings.xlsx",
    "Baseline Model Fit"
      )
daily_TOWT_no_occ_expected$model_fit <- NULL

# Read in processed data for testing
eload_file <-
  "Millenium Tower/Daily TOWT without occupancy/Normalized Savings/Processed Data/Processed Eload - Electric - kWh.xlsx"
temp_file <-
  "Millenium Tower/Daily TOWT without occupancy/Normalized Savings/Processed Data/Processed Temp.xlsx"
eload_data <- readxl::read_xlsx(eload_file)
temp_data <- readxl::read_xlsx(temp_file)

# MODELING DATES & INETRVAL (choose a section based on your end goal) ------
baseline_start_date <- "01/01/2019 00:00"
baseline_end_date <- "12/31/2019 23:45"

performance_start_date <- "01/01/2020 00:00"
performance_end_date <- "12/31/2020 23:45"

convert_to_data_interval <- "Daily"
modeling_algorithm <- "TOWT"

# Create Baseline Dataframes
daily_TOWT_no_occ_calculated <-
  create_dataframe(
    eload_data = eload_data,
    temp_data = temp_data,
    operating_mode_data = NULL,
    additional_independent_variables = NULL,
    additional_variable_aggregation = "median",
    start_date = baseline_start_date,
    end_date = baseline_end_date,
    convert_to_data_interval = convert_to_data_interval,
    temp_balancepoint = 65
  )

# Check col names
expect_equal(colnames(daily_TOWT_no_occ_calculated),
             colnames(daily_TOWT_no_occ_expected))

# Check length
expect_equal(length(daily_TOWT_no_occ_calculated$eload),
  length(daily_TOWT_no_occ_expected$eload))

# Compare data frames
expect_equal(daily_TOWT_no_occ_calculated, daily_TOWT_no_occ_expected)

# Check col sum
expect_equal(sum(daily_TOWT_no_occ_calculated$eload),
              sum(daily_TOWT_no_occ_expected$eload))

# Check # of NAs
expect_equal(
  is.na(daily_TOWT_no_occ_calculated$eload),
  is.na(daily_TOWT_no_occ_expected$eload)
)
