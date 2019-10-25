#' Add operating mode information to training and prediction dataframes when available
#'
#'
#' @param dataframe Training or Performance period dataframe, an object that has been created by the function create_dataframe()
#' @param operating_mode_data dataframe specifying indicator variables. The timestamp column must be labeled as 'time'
#'
#' @return a dataframe with operating mode data information, when available
#' @export
#'
#'

add_operating_mode_data <- function(dataframe = NULL, operating_mode_data = NULL){

  # if no operating mode data provided, return dataframe
  if(is.null(operating_mode_data)){
   return(dataframe)
  }

  # check data intervals of the datasets
  nterval_dataframe <- difftime(dataframe$time[2], dataframe$time[1], units = "min")
  nterval_operating_mode <- difftime(operating_mode_data$time[2], operating_mode_data$time[1], units = "min")

  if (nterval_dataframe < 60) {
    data_interval_dataframe <- "less than 60-min"
  } else if (nterval_dataframe == 60) {
    data_interval_dataframe <- "Hourly"
  } else if (nterval_dataframe == 1440) {
    data_interval_dataframe <- "Daily"
  } else if (nterval_dataframe > 2880) {
    data_interval_dataframe <- "Monthly"
  }

  if (nterval_operating_mode < 60) {
    data_interval_operating_mode <- "less than 60-min"
  } else if (nterval_operating_mode == 60) {
    data_interval_operating_mode <- "Hourly"
  } else if (nterval_operating_mode == 1440) {
    data_interval_operating_mode <- "Daily"
  } else if (nterval_operating_mode > 2880) {
    data_interval_operating_mode <- "Monthly"
  }


  if (data_interval_dataframe != data_interval_operating_mode){
    stop("The time intervals of the dataframe and the operating mode data are different. Please upload datasets with the same time interval")
  }

  data_with_operating_mode <- dplyr::inner_join(dataframe, operating_mode_data, by = "time")
  data_with_operating_mode <- distinct(data_with_operating_mode)

}

