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

# Create Models (SLR, 3PC, 3PH, 4P, 5P, HDD, CDD, HDD_CDD, TOW, TOWT) -----
##SLR Models
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

##TOWT Models
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

## CP Models
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

## HDD_CDD Models
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

#Create list of all model outputs for testing ----
all_models <-
  c(SLR_model_list, TOWT_model_list, CP_model_list, DD_model_list)
all_models_names <- c(names(all_models))

#Calculate_summary_statistics -----
summ_stats_list <-
  vector(mode = "list", length = length(all_models))
names(summ_stats_list) <- names(all_models)
for (i in 1:length(all_models)) {
  summ_stats_list[[i]] <-
    calculate_summary_statistics(modeled_data_obj = all_models[[i]])
}

#Calculate_model_predictions -----
all_model_predictions <-
  vector(mode = "list", length = length(all_models))
names(all_model_predictions) <- names(all_models)

for (i in 1:length(all_models)) {
  if (all_models[[i]]$model_input_options$chosen_modeling_interval == "Hourly") {
    training_data_predictions <- hourly_baseline
    prediction_data_predictions <- hourly_performance
  }
  if (all_models[[i]]$model_input_options$chosen_modeling_interval == "Daily") {
    training_data_predictions <- daily_baseline
    prediction_data_predictions <- daily_performance
  }
  if (all_models[[i]]$model_input_options$chosen_modeling_interval == "Monthly") {
    training_data_predictions <- monthly_baseline
    prediction_data_predictions <- monthly_performance
  }
  modeled_object_predictions <- all_models[[i]]
  all_model_predictions[[i]] <-
    calculate_model_predictions(
      training_data = training_data_predictions,
      prediction_data = prediction_data_predictions,
      modeled_object = modeled_object_predictions
    )
}

#Calculate_savings_and_uncertainty -----
savs_and_uncert_list <-
  vector(mode = "list", length = length(all_models))
names(savs_and_uncert_list) <- names(all_models)
for (i in 1:length(all_models)) {
  savs_and_uncert_list[[i]] <- calculate_savings_and_uncertainty(
    prediction_df = all_model_predictions[[i]],
    modeled_object = all_models[[i]],
    model_summary_statistics = summ_stats_list[[i]]
  )
}

#Calculate_coverage Function Testing -----
expected_coverage_daily <-
  calculate_coverage(
    dataframe = daily_baseline,
    ref_temp_data = tmy3_data,
    outlier_threshold = 1,
    extrapolation_limit = .05
  )
expected_coverage_hourly <-
  calculate_coverage(
    dataframe = hourly_baseline,
    ref_temp_data = tmy3_data,
    outlier_threshold = 1,
    extrapolation_limit = .05
  )

#Calculate_norm_savings_and_uncertainty -----
#Create performance model and summ stats for testing
performance_SLR_model_daily <-
  model_with_SLR(training_data = daily_performance, model_input_options = SLR_model_inputs)
performance_stats <-
  calculate_summary_statistics(modeled_data_obj = performance_SLR_model_daily)

expected_norm_savs_and_uncert <-
  calculate_norm_savings_and_uncertainty(
    baseline_model = SLR_model_daily,
    baseline_stats = summ_stats_list[["SLR_model_daily"]],
    performance_model = performance_SLR_model_daily,
    performance_stats = performance_stats,
    normalized_weather = tmy3_data
  )

#Aggregate Function -----
aggregated_expected <-
  aggregate(
    eload_data = eload_data,
    temp_data = temp_data,
    convert_to_data_interval = "Daily"
  )

#Align_data Function -----
eload_aligned_end <- align_data(eload_data, "end")
temp_aligned_start <- align_data(temp_data, "start")

#Assign_model_inputs Function -----
model_inputs_test <-
  assign_model_inputs(regression_type = "SLR")




