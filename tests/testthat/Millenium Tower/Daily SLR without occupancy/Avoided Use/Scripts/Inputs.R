

# This script is written to assist you in identifying the best model for your data.
# In order to ensure a smooth run, please read the instructions below before running any commands

# FOLDER STRUCTURE -----

## Your project should have the following folder structure:

### - As.Received (All customer data is stored here)
### - Raw Data
### - Processed Data (All analysis-ready input data is stored here)
###
###   See example filenames below
###   - Processed Temp (column headers: 'time' and 'temp')
###   - Processed Eload - Electric - kWh (column headers: 'time' and 'eload)
###   Note: data column headers are case-sensitive
###
### - Scripts (All scripts, including this one, should be saved here)
### - Results (All analysis results will be exported here)


# DATA INPUT TIMESTAMP FORMATTING -----

### Please check the input data files' timestamps before proceeding. 
### Make sure that the timestamps are formatted correctly and are not text values.
### One way to check this is to plot the raw data.
### If the x-axis is a timeseries, you can proceed. Else, please format your data appropriately.

# SETUP -----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # anchor to the location where this script is saved

# METADATA -----
project_name <- "Millennium Tower"
program_name <- "EEAS"

# FILE LOCATION AND NAMES (w.r.t Root Directory)
setwd('..')

eload_file <- "Processed Data/Processed Eload - Electric - kWh.xlsx"
temp_file <- "Processed Data/Processed Temp.xlsx"
additional_variable_data_file <- NULL

# DATA CHARACTERIZATION -----

utility <- "Electricity"
utility_units <- "kWh"
utility_source <- "Seattle City & Light"
utility_data_received_date <- "02/02/2021"

weather_station <- "Boeing Airfield"
distance_to_site <- "5 mi"
data_download_date <- "02/08/2021"
TMY_data_available <- "Yes"

additional_variable_aggregation <- c(median) # Aggregation functions allowed: sum, mean, median. 
# The number of functions should match the number of additional variables in the additional_variable_data_file

# MODELING DATES & INETRVAL (choose a section based on your end goal) ------ 

baseline_start_date <- "01/01/2019 00:00" 
baseline_end_date <- "12/31/2019 23:45"

performance_start_date <- "01/01/2020 00:00" 
performance_end_date <- "12/31/2020 23:45"

convert_to_data_interval <- "Daily" 

modeling_algorithm <- "SLR"

expected_avoided_use <- 1089101

# RUN ANALYSIS ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rmarkdown::render(input = "Avoided Use.Rmd", output_format = "word_document", output_dir = "../Results/") # Run for Avoided Energy Use
