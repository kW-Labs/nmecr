## ---- echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------
require(nmecr)
require(plotly)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Helper Functions.R")


## ---- echo = FALSE, message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------------------
# Data Read In
setwd('..')
temp_data <- readxl::read_xlsx(temp_file)
eload_data <- readxl::read_xlsx(eload_file)

# Determine valid analysis intervals for each energy source

valid_analysis_intervals <-  determine_lowest_interval(eload_data)

# Create Dataframes for Analysis

dfs <- purrr::pmap(.l = list(eload_data = list(eload_data),
                             temp_data = list(temp_data),
                             convert_to_data_interval = valid_analysis_intervals),
                   .f = nmecr::create_dataframe,
                   start_date = start_date,
                   end_date = end_date)

names(dfs) <- valid_analysis_intervals

# Run Models

models <- purrr::map2(.x = dfs, .y = names(dfs), .f = run_all_models)

results_df <- bind_rows(lapply(models, `[[`, 1))
names(results_df) <- c('Model', 'Interval', 'R^2', 'Adjusted R^2', 'CV(RMSE) %', 'NDBE %', 'NMBE %', '#Parameters', 'dof', 'Savings Fraction', 'Uncertainty for 10% Savings', 'Savings Frac for 50% Unc', 'CL')

results_df <- results_df[, c('Model', 'Interval', 'R^2', 'Adjusted R^2', 'CV(RMSE) %', 'NMBE %', 'Uncertainty for 10% Savings', 'Savings Frac for 50% Unc', 'CL')]

results_df[, c(3:8)] <- lapply(results_df[, c(3:8)], as.numeric)
results_df[, c(3:8)] <- lapply(results_df[, c(3:8)], round, 2)
results_df[, 6] <- lapply(results_df[, 6], format, nsmall = 3)

if ("Daily" %in% results_df$Interval) { # first check for daily
  
  chosen_model <- results_df %>%
    filter(Interval == "Daily") %>%
    filter(`CV(RMSE) %` == min(`CV(RMSE) %`))
  
  if (nrow(chosen_model) > 1){
    chosen_model <- chosen_model %>%
    filter(`Uncertainty for 10% Savings` == min(`Uncertainty for 10% Savings`))
  }
  
  if (nrow(chosen_model) > 1){
    chosen_model <- chosen_model[1,]
  }
  
  chosen_algo <- chosen_model$Model
  
  model <- models$Daily$model_fit_df[, c('time', 'temp', 'HDD', 'CDD', 'eload', paste0(chosen_algo, '_fit'))]
  colnames(model) <- c('time', 'temp', 'HDD', 'CDD', 'eload', 'model_fit')
  
} else if ("Monthly" %in% results_df$Interval) { # next check for monthly
  
  chosen_model <- results_df %>%
    filter(Interval == "Daily") %>%
    filter(`CV(RMSE) %` == min(`CV(RMSE) %`))
  
    
  if (nrow(chosen_model) > 1){
    chosen_model <- chosen_model %>%
    filter(`Uncertainty for 10% Savings` == min(`Uncertainty for 10% Savings`))
  }
  
  if (nrow(chosen_model) > 1){
    chosen_model <- chosen_model[1,]
  }
  
  chosen_algo <- chosen_model$Model
  
  model <- models$Daily$model_fit_df[, c('time', 'temp', 'HDD', 'CDD', 'eload', paste0(chosen_algo, '_fit'))]
  colnames(model) <- c('time', 'temp', 'HDD', 'CDD', 'HDD_perday', 'CDD_perday', 'eload', 'eload_perday', 'model_fit')
  
}

# Write Out Results

model_dfs <- lapply(models, `[[`, 2)

result_list <- rlist::list.append(model_dfs, "Summary" = results_df)

openxlsx::write.xlsx(result_list, paste0('Results/', project_name, '-Predictability Analysis.xlsx'))
















## ---- echo = FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------------------
knitr::purl(input = 'Predictability.Rmd', output = 'Predictability.R') # Delete this from the output R script

