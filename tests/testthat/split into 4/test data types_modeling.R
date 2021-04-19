# Source main file
source("test data types_main.R")

# Create Models (SLR, 3PC, 3PH, 4P, 5P, HDD, CDD, HDD_CDD, TOW, TOWT) -----

## model_with_SLR Function Testing
SLR_model_inputs <-
  assign_model_inputs(regression_type = "SLR", day_normalized = FALSE)
SLR_model_daily <-
  model_with_SLR(training_data = daily_baseline, model_input_options = SLR_model_inputs)
SLR_model_hourly <-
  model_with_SLR(training_data = hourly_baseline, model_input_options = SLR_model_inputs)
SLR_model_monthly <-
  model_with_SLR(training_data = monthly_baseline, model_input_options = SLR_model_inputs)

SLR_model_list <-
  list(SLR_model_hourly,
       SLR_model_daily,
       SLR_model_monthly)
SLR_model_names <-
  c("SLR_model_hourly",
    "SLR_model_daily",
    "SLR_model_monthly")
names(SLR_model_list) <- SLR_model_names

for (i in 1:length(SLR_model_list)) {
  expect_type(SLR_model_list[[!!SLR_model_names[i]]], "list")
  expect_type(SLR_model_list[[!!SLR_model_names[i]]]$model, "list")
  expect_s3_class(SLR_model_list[[!!SLR_model_names[i]]]$model_stats, "data.frame")
  expect_s3_class(SLR_model_list[[!!SLR_model_names[i]]]$training_data, "data.frame")
  expect_type(SLR_model_list[[!!SLR_model_names[i]]]$model_input_options, "list")
}

## model_with_TOWT Function Testing (No occupancy input data)
TOWT_model_inputs <-
  assign_model_inputs(regression_type = "TOWT", day_normalized = FALSE)
TOWT_model_inputs_timescale <-
  assign_model_inputs(
    timescale_days = 14,
    regression_type = "TOWT",
    day_normalized = FALSE
  )
TOW_model_inputs <-
  assign_model_inputs(regression_type = "TOW", day_normalized = FALSE)

TOWT_model_daily <-
  model_with_TOWT(
    training_data = daily_baseline,
    prediction_data = daily_performance,
    model_input_options = TOWT_model_inputs,
    occupancy_info =  NULL
  )
TOWT_model_hourly <-
  model_with_TOWT(
    training_data = hourly_baseline,
    prediction_data = hourly_performance,
    model_input_options = TOWT_model_inputs,
    occupancy_info = NULL
  )
TOWT_model_daily_timescale <-
  model_with_TOWT(
    training_data = daily_baseline,
    prediction_data = daily_performance,
    model_input_options = TOWT_model_inputs_timescale,
    occupancy_info = NULL
  )
TOWT_model_hourly_timescale <-
  model_with_TOWT(
    training_data = hourly_baseline,
    prediction_data =
      hourly_performance,
    model_input_options =
      TOWT_model_inputs_timescale,
    occupancy_info =
      NULL
  )
TOW_model_daily <-
  model_with_TOWT(
    training_data = daily_baseline,
    prediction_data = daily_performance,
    model_input_options = TOW_model_inputs,
    occupancy_info = NULL
  )
TOW_model_hourly <-
  model_with_TOWT(
    training_data = hourly_baseline,
    prediction_data = hourly_performance,
    model_input_options = TOW_model_inputs,
    occupancy_info = NULL
  )
TOWT_model_list <-
  list(
    TOWT_model_hourly,
    TOWT_model_daily,
    TOWT_model_hourly_timescale,
    TOWT_model_daily_timescale
  )
TOW_model_list <- list(TOW_model_hourly, TOW_model_daily)
TOWT_model_names <-
  c(
    "TOWT_model_hourly",
    "TOWT_model_daily",
    "TOWT_model_hourly_timescale",
    "TOWT_model_daily_timescale"
  )
TOW_model_names <- c("TOW_model_hourly", "TOW_model_daily")
names(TOWT_model_list) <- TOWT_model_names
names(TOW_model_list) <- TOW_model_names

for (i in 1:length(TOWT_model_list)) {
  expect_type(TOWT_model_list[[!!TOWT_model_names[i]]], "list")
  expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$training_data, "data.frame")
  expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$prediction_data, "data.frame")
  expect_type(TOWT_model_list[[!!TOWT_model_names[i]]]$model_occupied, "list")
  expect_type(TOWT_model_list[[!!TOWT_model_names[i]]]$model_unoccupied, "list")
  expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$model_occupied_stats, "data.frame")
  expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$model_unoccupied_stats, "data.frame")
  expect_type(TOWT_model_list[[!!TOWT_model_names[i]]]$model_input_options, "list")
  if (i == length(TOWT_model_list)) {
    ## we can consider adding if statements like these to print at the end of for loops, just an idea
    print("iterated through TOWT test")
  }
}
for (i in 1:length(TOW_model_list)) {
  expect_type(TOW_model_list[[!!TOW_model_names[i]]], "list")
  expect_s3_class(TOW_model_list[[!!TOW_model_names[i]]]$training_data, "data.frame")
  expect_s3_class(TOW_model_list[[!!TOW_model_names[i]]]$prediction_data, "data.frame")
  expect_type(TOW_model_list[[!!TOW_model_names[i]]]$model_occupied, "list")
  expect_s3_class(TOW_model_list[[!!TOW_model_names[i]]]$model_occupied_stats, "data.frame")
  expect_type(TOW_model_list[[!!TOW_model_names[i]]]$model_input_options, "list")
}

## Model_with_CP Function Testing
model_inputs_3PC <-
  assign_model_inputs(regression_type = "3PC", day_normalized = FALSE)
model_inputs_3PH <-
  assign_model_inputs(regression_type = "3PH", day_normalized = FALSE)
model_inputs_4P <-
  assign_model_inputs(regression_type = "4P", day_normalized = FALSE)
model_inputs_5P <-
  assign_model_inputs(regression_type = "5P", day_normalized = FALSE)

hourly_3PC_model <-
  model_with_CP(training_data = hourly_baseline, model_input_options = model_inputs_3PC)
daily_3PC_model <-
  model_with_CP(training_data = daily_baseline, model_input_options = model_inputs_3PC)
monthly_3PC_model <-
  model_with_CP(training_data = monthly_baseline, model_input_options = model_inputs_3PC)
hourly_3PH_model <-
  model_with_CP(training_data = hourly_baseline, model_input_options = model_inputs_3PH)
daily_3PH_model <-
  model_with_CP(training_data = daily_baseline, model_input_options = model_inputs_3PH)
monthly_3PH_model <-
  model_with_CP(training_data = monthly_baseline, model_input_options = model_inputs_3PH)
hourly_4P_model <-
  model_with_CP(training_data = hourly_baseline, model_input_options = model_inputs_4P)
daily_4P_model <-
  model_with_CP(training_data = daily_baseline, model_input_options = model_inputs_4P)
monthly_4P_model <-
  model_with_CP(training_data = monthly_baseline, model_input_options = model_inputs_4P)
hourly_5P_model <-
  model_with_CP(training_data = hourly_baseline, model_input_options = model_inputs_5P)
daily_5P_model <-
  model_with_CP(training_data = daily_baseline, model_input_options = model_inputs_5P)
monthly_5P_model <-
  model_with_CP(training_data = monthly_baseline, model_input_options = model_inputs_5P)

CP_model_list <-
  list(
    hourly_3PC_model,
    daily_3PC_model,
    monthly_3PC_model,
    hourly_3PH_model,
    daily_3PH_model,
    monthly_3PH_model,
    hourly_4P_model,
    daily_4P_model,
    monthly_4P_model,
    hourly_5P_model,
    daily_5P_model,
    monthly_5P_model
  )
CP_model_names <-
  c(
    "hourly_3PC_model",
    "daily_3PC_model",
    "monthly_3PC_model",
    "hourly_3PH_model",
    "daily_3PH_model",
    "monthly_3PH_model",
    "hourly_4P_model",
    "daily_4P_model",
    "monthly_4P_model",
    "hourly_5P_model",
    "daily_5P_model",
    "monthly_5P_model"
  )
names(CP_model_list) <- CP_model_names

for (i in 1:length(CP_model_list)) {
  expect_type(CP_model_list[[!!CP_model_names[i]]], "list")
  expect_type(CP_model_list[[!!CP_model_names[i]]]$model, "list")
  expect_s3_class(CP_model_list[[!!CP_model_names[i]]]$model_stats, "data.frame")
  expect_s3_class(CP_model_list[[!!CP_model_names[i]]]$training_data, "data.frame")
  expect_type(CP_model_list[[!!CP_model_names[i]]]$model_input_options, "list")
}

## Model_with_HDD_CDD Function Testing
model_inputs_HDD_CDD <-
  assign_model_inputs(regression_type = "HDD-CDD", day_normalized = FALSE)
model_inputs_HDD <-
  assign_model_inputs(regression_type = "HDD", day_normalized = FALSE)
model_inputs_CDD <-
  assign_model_inputs(regression_type = "CDD", day_normalized = FALSE)
HDD_bp <- 65
CDD_bp <- 65

HDD_CDD_model_daily <-
  model_with_HDD_CDD(
    training_data = daily_baseline,
    model_input_options = model_inputs_HDD_CDD,
    HDD_balancepoint = HDD_bp,
    CDD_balancepoint =  CDD_bp
  )
HDD_model_daily <-
  model_with_HDD_CDD(
    training_data = daily_baseline,
    model_input_options = model_inputs_HDD,
    HDD_balancepoint = HDD_bp,
    CDD_balancepoint = CDD_bp
  )
CDD_model_daily <-
  model_with_HDD_CDD(
    training_data = daily_baseline,
    model_input_options = model_inputs_CDD,
    HDD_balancepoint = HDD_bp,
    CDD_balancepoint = CDD_bp
  )
HDD_CDD_model_monthly <-
  model_with_HDD_CDD(
    training_data = monthly_baseline,
    model_input_options = model_inputs_HDD_CDD,
    HDD_balancepoint = HDD_bp,
    CDD_balancepoint = CDD_bp
  )
HDD_model_monthly <-
  model_with_HDD_CDD(
    training_data = monthly_baseline,
    model_input_options = model_inputs_HDD,
    HDD_balancepoint = HDD_bp,
    CDD_balancepoint = CDD_bp
  )
CDD_model_monthly <-
  model_with_HDD_CDD(
    training_data = monthly_baseline,
    model_input_options = model_inputs_CDD,
    HDD_balancepoint = HDD_bp,
    CDD_balancepoint = CDD_bp
  )

DD_model_list <-
  list(
    HDD_CDD_model_daily,
    HDD_CDD_model_monthly,
    HDD_model_daily,
    HDD_model_monthly,
    CDD_model_daily,
    CDD_model_monthly
  )
DD_model_names <-
  c(
    "HDD_CDD_model_daily",
    "HDD_CDD_model_monthly",
    "HDD_model_daily",
    "HDD_model_monthly",
    "CDD_model_daily",
    "CDD_model_monthly"
  )
names(DD_model_list) <- DD_model_names

for (i in 1:length(DD_model_list)) {
  expect_type(DD_model_list[[!!DD_model_names[i]]], "list")
  expect_type(DD_model_list[[!!DD_model_names[i]]]$model, "list")
  expect_s3_class(DD_model_list[[!!DD_model_names[i]]]$training_data, "data.frame")
  expect_s3_class(DD_model_list[[!!DD_model_names[i]]]$model_stats, "data.frame")
  expect_type(DD_model_list[[!!DD_model_names[i]]]$model_input_options, "list")
}

#Create list of all model outputs for testing ----
all_models <-
  c(SLR_model_list, TOWT_model_list, CP_model_list, DD_model_list)
all_models_names <- c(names(all_models))

