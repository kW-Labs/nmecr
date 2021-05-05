context("Modeled Objects")
source("test data types_definitions.R")

test_that("SLR model objects are intact", {
  for (i in 1:length(SLR_model_list)) {
    expect_type(SLR_model_list[[!!SLR_model_names[i]]], "list")
    expect_type(SLR_model_list[[!!SLR_model_names[i]]]$model, "list")
    expect_s3_class(SLR_model_list[[!!SLR_model_names[i]]]$model_stats, "data.frame")
    expect_s3_class(SLR_model_list[[!!SLR_model_names[i]]]$training_data, "data.frame")
    expect_type(SLR_model_list[[!!SLR_model_names[i]]]$model_input_options, "list")
  }
})

test_that("TOWT model objects are intact", {
  for (i in 1:length(TOWT_model_list)) {
    expect_type(TOWT_model_list[[!!TOWT_model_names[i]]], "list")
    expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$training_data, "data.frame")
    expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$prediction_data, "data.frame")
    expect_type(TOWT_model_list[[!!TOWT_model_names[i]]]$model_occupied, "list")
    expect_type(TOWT_model_list[[!!TOWT_model_names[i]]]$model_unoccupied, "list")
    expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$model_occupied_stats, "data.frame")
    expect_s3_class(TOWT_model_list[[!!TOWT_model_names[i]]]$model_unoccupied_stats, "data.frame")
    expect_type(TOWT_model_list[[!!TOWT_model_names[i]]]$model_input_options, "list")
  }
})


test_that("TOW model objects are intact", {
  for (i in 1:length(TOW_model_list)) {
    expect_type(TOW_model_list[[!!TOW_model_names[i]]], "list")
    expect_s3_class(TOW_model_list[[!!TOW_model_names[i]]]$training_data, "data.frame")
    expect_s3_class(TOW_model_list[[!!TOW_model_names[i]]]$prediction_data, "data.frame")
    expect_type(TOW_model_list[[!!TOW_model_names[i]]]$model_occupied, "list")
    expect_s3_class(TOW_model_list[[!!TOW_model_names[i]]]$model_occupied_stats, "data.frame")
    expect_type(TOW_model_list[[!!TOW_model_names[i]]]$model_input_options, "list")
  }
})


test_that("CP model objects are intact", {
  for (i in 1:length(CP_model_list)) {
    expect_type(CP_model_list[[!!CP_model_names[i]]], "list")
    expect_type(CP_model_list[[!!CP_model_names[i]]]$model, "list")
    expect_s3_class(CP_model_list[[!!CP_model_names[i]]]$model_stats, "data.frame")
    expect_s3_class(CP_model_list[[!!CP_model_names[i]]]$training_data, "data.frame")
    expect_type(CP_model_list[[!!CP_model_names[i]]]$model_input_options, "list")
  }
})

test_that("HDD-CDD model objects are intact", {
  for (i in 1:length(DD_model_list)) {
    expect_type(DD_model_list[[!!DD_model_names[i]]], "list")
    expect_type(DD_model_list[[!!DD_model_names[i]]]$model, "list")
    expect_s3_class(DD_model_list[[!!DD_model_names[i]]]$training_data, "data.frame")
    expect_s3_class(DD_model_list[[!!DD_model_names[i]]]$model_stats, "data.frame")
    expect_type(DD_model_list[[!!DD_model_names[i]]]$model_input_options, "list")
  }
})
