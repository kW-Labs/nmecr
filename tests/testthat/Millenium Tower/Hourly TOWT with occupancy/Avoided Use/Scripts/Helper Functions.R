# Predictability Helper Functions ------

# Determine valid analysis intervals for each energy source

determine_lowest_interval <- function(eload) {
  nterval <-  median(diff(as.numeric(eload$time)))/60
  
  if (nterval == 15 | nterval == 60){
    valid <- list("Hourly", "Daily", "Monthly")
  } else if (nterval == 1440) {
    valid <- list("Daily", "Monthly")
  } else {
    valid <- list("Monthly")
  }
  
  #names(valid) <- 
  
  return (valid)
}


# Run All Models - lower level function

run_all_models <- function(df, name) {
  
  results <- data.frame(matrix(nrow = 0, ncol = 11))
  colnames(results) <- c("Model", "R_Squared", "CVRMSE %", "NDBE %", "NMBE %", "#Parameters", "deg_of_freedom",
                         "savings_fraction", "savings_uncertainty", "savings_frac_for_50_pct_uncertainty",
                         "confidence_level")
  
  model_fit_df <- as.data.frame(df)
  
  try({
    SLR <- nmecr::model_with_SLR(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'SLR'))
    SLR_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = SLR)
    SLR_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = SLR, model_summary_statistics = SLR_stats, confidence_level = 90)
    SLR_results <- bind_cols(SLR_stats, SLR_uncertainty$savings_summary_df)
    SLR_results$Model <- "SLR"
  }, silent = TRUE)
  
  if (exists("SLR_results")) {
    results <- bind_rows(SLR_results)
    SLR$training_data <- SLR$training_data %>%
      rename("SLR_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(SLR$training_data, time, SLR_fit), by = "time")
  }
  
  
  try({
    Three_PH <- nmecr::model_with_CP(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'Three Parameter Heating'))
    Three_PH_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = Three_PH)
    Three_PH_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = Three_PH, model_summary_statistics = Three_PH_stats, confidence_level = 90)
    Three_PH_results <- bind_cols(Three_PH_stats, Three_PH_uncertainty$savings_summary_df)
    Three_PH_results$Model <- "Three_PH"
  }, silent = TRUE)
  
  if (exists("Three_PH_results")) {
    results <- results %>%
      bind_rows(Three_PH_results)
    Three_PH$training_data <- Three_PH$training_data %>%
      rename("Three_PH_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(Three_PH$training_data, time, Three_PH_fit), by = "time")
  }
  
  
  try({
    Three_PC <- nmecr::model_with_CP(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'Three Parameter Cooling'))
    Three_PC_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = Three_PC)
    Three_PC_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = Three_PC, model_summary_statistics = Three_PC_stats, confidence_level = 90)
    Three_PC_results <- bind_cols(Three_PC_stats, Three_PC_uncertainty$savings_summary_df)
    Three_PC_results$Model <- "Three_PC"
  }, silent = TRUE)
  
  if (exists("Three_PC_results")) {
    results <- results %>%
      bind_rows(Three_PC_results)
    Three_PC$training_data <- Three_PC$training_data %>%
      rename("Three_PC_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(Three_PC$training_data, time, Three_PC_fit), by = "time")
  }
  
  
  try({
    Four_P <- nmecr::model_with_CP(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'Four Parameter Linear Model'))
    Four_P_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = Four_P)
    Four_P_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = Four_P, model_summary_statistics = Four_P_stats, confidence_level = 90)
    Four_P_results <- bind_cols(Four_P_stats, Four_P_uncertainty$savings_summary_df)
    Four_P_results$Model <- "Four_P"
  }, silent = TRUE)
  
  if (exists("Four_P_results")) {
    results <- results %>%
      bind_rows(Four_P_results)
    Four_P$training_data <- Four_P$training_data %>%
      rename("Four_P_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(Four_P$training_data, time, Four_P_fit), by = "time")
  }
  
  # 
  # try({
  #   
  #   Five_P <- nmecr::model_with_CP(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'Five Parameter Linear Model', initial_breakpoints = c(55,65)))
  #   Five_P_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = Five_P)
  #   Five_P_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = Five_P, model_summary_statistics = Five_P_stats, confidence_level = 90)
  #   Five_P_results <- bind_cols(Five_P_stats, Five_P_uncertainty$savings_summary_df)
  #   Five_P_results$Model <- "Five_P"
  # }, silent = TRUE)
  # 
  # if (exists("Five_P_results")) {
  #   results <- results %>%
  #     bind_rows(Five_P_results)
  #   df_name <- paste("Five P", name)
  #   model_fit_dfs[[df_name]] <- Five_P$training_data
  # }
  # 
  
  try({
    
    HDD <- nmecr::model_with_HDD_CDD(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'HDD Regression'))
    HDD_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = HDD)
    HDD_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = HDD, model_summary_statistics = HDD_stats, confidence_level = 90)
    HDD_results <- bind_cols(HDD_stats, HDD_uncertainty$savings_summary_df)
    HDD_results$Model <- "HDD"
  }, silent = TRUE)
  
  if (exists("HDD_results")) {
    results <- results %>%
      bind_rows(HDD_results)
    HDD$training_data <- HDD$training_data %>%
      rename("HDD_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(HDD$training_data, time, HDD_fit), by = "time")
  }
  
  try({
    
    CDD <- nmecr::model_with_HDD_CDD(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'CDD Regression'))
    CDD_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = CDD)
    CDD_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = CDD, model_summary_statistics = CDD_stats, confidence_level = 90)
    CDD_results <- bind_cols(CDD_stats, CDD_uncertainty$savings_summary_df)
    CDD_results$Model <- "CDD"
  }, silent = TRUE)
  
  if (exists("CDD_results")) {
    results <- results %>%
      bind_rows(CDD_results)
    CDD$training_data <- CDD$training_data %>%
      rename("CDD_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(CDD$training_data, time, CDD_fit), by = "time")
  }
  
  try({
    
    HDD_CDD <- nmecr::model_with_HDD_CDD(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'HDD-CDD Multivariate Regression'))
    HDD_CDD_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = HDD_CDD)
    HDD_CDD_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = HDD_CDD, model_summary_statistics = HDD_CDD_stats, confidence_level = 90)
    HDD_CDD_results <- bind_cols(HDD_CDD_stats, HDD_CDD_uncertainty$savings_summary_df)
    HDD_CDD_results$Model <- "HDD_CDD"
  }, silent = TRUE)
  
  if (exists("HDD_CDD_results")) {
    results <- results %>%
      bind_rows(HDD_CDD_results)
    HDD_CDD$training_data <- HDD_CDD$training_data %>%
      rename("HDD_CDD_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(HDD_CDD$training_data, time, HDD_CDD_fit), by = "time")
  }
  
  try({
    
    TOW <- nmecr::model_with_TOWT(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'TOW'))
    TOW_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = TOW)
    TOW_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = TOW, model_summary_statistics = TOW_stats, confidence_level = 90)
    TOW_results <- bind_cols(TOW_stats, TOW_uncertainty$savings_summary_df)
    TOW_results$Model <- "TOW"
  }, silent = TRUE)
  
  if (exists("TOW_results")) {
    results <- results %>%
      bind_rows(TOW_results)
    TOW$training_data <- TOW$training_data %>%
      rename("TOW_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(TOW$training_data, time, TOW_fit), by = "time")
  }
  
  try({
    
    TOWT <- nmecr::model_with_TOWT(training_data = df, model_input_options = nmecr::assign_model_inputs(regression_type = 'TOWT'))
    TOWT_stats <- nmecr::calculate_summary_statistics(modeled_data_obj = TOWT)
    TOWT_uncertainty <- nmecr::calculate_savings_and_uncertainty(savings_fraction = 0.10, modeled_object = TOWT, model_summary_statistics = TOWT_stats, confidence_level = 90)
    TOWT_results <- bind_cols(TOWT_stats, TOWT_uncertainty$savings_summary_df)
    TOWT_results$Model <- "TOWT"
  }, silent = TRUE)
  
  if (exists("TOWT_results")) {
    results <- results %>%
      bind_rows(TOWT_results)
    TOWT$training_data <- TOWT$training_data %>%
      rename("TOWT_fit" = model_fit)
    model_fit_df <- model_fit_df %>%
      dplyr::left_join(select(TOWT$training_data, time, TOWT_fit), by = "time")
  }
  
  results <- results %>%
    mutate("Interval" = name)
  
  results <- results %>%
    select(Model, Interval,  everything())
  
  results <- results[order(results$`CVRMSE %`),]
  
  results_list <- list()
  results_list$results <- results
  results_list$model_fit_df <- model_fit_df
  
  return(results_list)
  
}

# Avoided Use Helper Functions -------

if (exists("modeling_algorithm")) {

  if (modeling_algorithm == 'SLR' | modeling_algorithm == 'Simple Linear Algorithm') {
    modeling_function <- nmecr::model_with_SLR
  } else if (modeling_algorithm == '3PC' | modeling_algorithm == 'Three Parameter Cooling' |
             modeling_algorithm == '3PH' | modeling_algorithm == 'Three Parameter Heating' |
             modeling_algorithm == '4P' | modeling_algorithm == 'Four Parameter Linear Regression' |
             modeling_algorithm == '5P' | modeling_algorithm == 'Five Parameter Linear Regression') {
    modeling_function <- nmecr::model_with_CP
  } else if (modeling_algorithm == 'HDD' | modeling_algorithm == 'HDD Regression' |
             modeling_algorithm == 'CDD' | modeling_algorithm == 'CDD Regression' |
             modeling_algorithm == 'HDD-CDD' | modeling_algorithm == 'HDD-CDD Multivariate Regression') {
    modeling_function <- nmecr::model_with_HDD_CDD
  } else {
    modeling_function <- nmecr::model_with_TOWT
  }
  
}

# Normalized Savings Helper Functions -----------

# FORMAT WEATHER COVERAGE SUMMARY

format_coverage_summary <- function(coverage_summary){
  
  summary1 <- coverage_summary[1:4,1:2]
  summary2 <- coverage_summary[1:4,3:4]
  
  names(summary1) <- c("Measure", "Value")
  names(summary2) <- c("Measure", "Value")
  
  summary <- dplyr::bind_rows(summary1, summary2)
  return(summary)
}

