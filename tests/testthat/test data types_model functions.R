# #Source Data
# source("test data types_definitions.R")

# model_with_SLR Function Testing
for (i in 1:length(SLR_model_list)) {
  expect_type(SLR_model_list[[!!SLR_model_names[i]]], "list")
  expect_type(SLR_model_list[[!!SLR_model_names[i]]]$model, "list")
  expect_s3_class(SLR_model_list[[!!SLR_model_names[i]]]$model_stats, "data.frame")
  expect_s3_class(SLR_model_list[[!!SLR_model_names[i]]]$training_data, "data.frame")
  expect_type(SLR_model_list[[!!SLR_model_names[i]]]$model_input_options, "list")
}

# model_with_TOWT Function Testing (No occupancy input data)
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

# Model_with_CP Function Testing
for (i in 1:length(CP_model_list)) {
  expect_type(CP_model_list[[!!CP_model_names[i]]], "list")
  expect_type(CP_model_list[[!!CP_model_names[i]]]$model, "list")
  expect_s3_class(CP_model_list[[!!CP_model_names[i]]]$model_stats, "data.frame")
  expect_s3_class(CP_model_list[[!!CP_model_names[i]]]$training_data, "data.frame")
  expect_type(CP_model_list[[!!CP_model_names[i]]]$model_input_options, "list")
}

# Model_with_HDD_CDD Function Testing
for (i in 1:length(DD_model_list)) {
  expect_type(DD_model_list[[!!DD_model_names[i]]], "list")
  expect_type(DD_model_list[[!!DD_model_names[i]]]$model, "list")
  expect_s3_class(DD_model_list[[!!DD_model_names[i]]]$training_data, "data.frame")
  expect_s3_class(DD_model_list[[!!DD_model_names[i]]]$model_stats, "data.frame")
  expect_type(DD_model_list[[!!DD_model_names[i]]]$model_input_options, "list")
}
