#' Heating/Cooling Degree Days algorithms using outside air temperature.
#'
#' \code{This function builds an energy use model using one of three algorithms: HDD-CDD Multivariate Regression (HDD-CDD), HDD Regression (HDD),
#' CDD Regression (CDD).}#'
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
#' @param HDD_balancepoint Heating specific balancepoint
#' @param CDD_balancepoint Cooling specific balancepoint
#'
#' @importFrom magrittr %>%
#'
#' @return a list with the following components:
#' \describe{
#'   \item{model}{an lm object}
#'   \item{training_data}{training dataframe along with the model_fit values}
#'   \item{model_input_options}{model_input_options from the input along with additional model specifications.}
#' }
#'
#' @export

model_with_HDD_CDD <- function(training_data = NULL, model_input_options = NULL, HDD_balancepoint = NULL, CDD_balancepoint = NULL){

  HDD <- CDD <- HDD_perday <- CDD_perday <- temp <- NULL # No visible binding for global variable

  training_data <- training_data[stats::complete.cases(training_data), ] # remove any incomplete observations

  nterval <-  stats::median(diff(as.numeric(training_data$time)))/60

  if (nterval == 15){
    nterval_value <- "15-min"
  } else if (nterval == 60){
    nterval_value <- "Hourly"
  } else if (nterval == 1440) {
    nterval_value <- "Daily"
  } else if (nterval >= 40320) {
    nterval_value <- "Monthly"
  }

  model_input_options$chosen_modeling_interval <- nterval_value

  if(model_input_options$chosen_modeling_interval == "15-min" | model_input_options$chosen_modeling_interval == "Hourly") {
    stop("Error: model_with_HDD_CDD cannot be used with sub-daily data.")
  }

  if(model_input_options$chosen_modeling_interval == "Daily") {

    # Calculate HDD CDD balancepoints if not given
    if (is.null(HDD_balancepoint)) {
      HDD_df <- training_data[, c("time", "temp", "HDD")] %>%
        dplyr::filter(HDD != 0) %>%
        dplyr::mutate(HDD_balancepoint = temp+HDD)
      HDD_temp <- stats::median(HDD_df$HDD_balancepoint)
    }

    if (is.null(CDD_balancepoint)) {
      CDD_df <- training_data[, c("time", "temp", "CDD")] %>%
        dplyr::filter(CDD != 0) %>%
        dplyr::mutate(CDD_balancepoint = temp-CDD)
      CDD_temp <- stats::median(CDD_df$CDD_balancepoint)
    }

    # If balancepoints are provided, overwrite HDD and CDD values inherited from create_dataframe
    if (! is.null(HDD_balancepoint)) {
      training_data$HDD <- HDD_balancepoint - training_data$temp
      training_data$HDD[training_data$HDD < 0 ] <- 0
      HDD_temp <- HDD_balancepoint # rename to a common name for use in variable names later on
    }

    if (! is.null(CDD_balancepoint)) {
      training_data$CDD <- training_data$temp - CDD_balancepoint
      training_data$CDD[training_data$CDD < 0 ] <- 0
      CDD_temp <- CDD_balancepoint # rename to a common name for use in variable names later on
    }

  } else if (model_input_options$chosen_modeling_interval == "Monthly") {

    # Calculate HDD CDD balancepoints if not given
    if (is.null(HDD_balancepoint)) {
      HDD_df <- training_data[, c("time", "temp", "HDD", "HDD_perday")] %>%
        dplyr::filter(HDD != 0) %>%
        dplyr::mutate(HDD_balancepoint = temp+HDD_perday)
      HDD_temp <- stats::median(HDD_df$HDD_balancepoint)
    }

    if (is.null(CDD_balancepoint)) {
      CDD_df <- training_data[, c("time", "temp", "CDD", "CDD_perday")] %>%
        dplyr::filter(CDD != 0) %>%
        dplyr::mutate(CDD_balancepoint = temp-CDD_perday)
      CDD_temp <- stats::median(CDD_df$CDD_balancepoint)
    }

    # If balancepoints are provided, overwrite HDD and CDD values inherited from create_dataframe
    if (! is.null(HDD_balancepoint)) {
      training_data$HDD_perday <- HDD_balancepoint - training_data$temp
      training_data$HDD_perday[training_data$HDD_perday < 0 ] <- 0
      training_data$HDD <- training_data$HDD_perday*training_data$days
      HDD_temp <- HDD_balancepoint # rename to a common name for use in variable names later on
    }

    if (! is.null(CDD_balancepoint)) {
      training_data$CDD_perday <- training_data$temp - CDD_balancepoint
      training_data$CDD_perday[training_data$CDD_perday < 0 ] <- 0
      training_data$CDD <- training_data$CDD_perday*training_data$days
      CDD_temp <- CDD_balancepoint # rename to a common name for use in variable names later on
    }


  }

  if (model_input_options$regression_type == "HDD-CDD Multivariate Regression" | model_input_options$regression_type == "HDD-CDD") {

    if (nterval_value == "Monthly") {

      if (model_input_options$day_normalized == TRUE){

        df <- training_data %>%
          dplyr::select(-c("time", "temp", "HDD", "CDD", "days"))

        df <- df %>%
          dplyr::select(dplyr::contains("_perday"))

        linregress <- stats::lm(eload_perday ~ ., data = df)

      } else {

        df <- training_data %>%
          dplyr::select(-c("time", "temp", "HDD_perday", "CDD_perday", "days"))

        df <- df %>%
          dplyr::select(! dplyr::contains("_perday"))

        linregress <- stats::lm(eload ~ ., data = df)
      }

    } else if (nterval_value == "Daily") {

      df <- training_data %>%
        dplyr::select(-c("time", "temp"))

      linregress <- stats::lm(eload ~ ., data = df)

    }

  } else if (model_input_options$regression_type == "HDD Regression" | model_input_options$regression_type == "HDD") {

    if (nterval_value == "Monthly") {

      if (model_input_options$day_normalized == TRUE){

        df <- training_data %>%
          dplyr::select(-c("time", "temp", "HDD", "CDD", "days", "CDD_perday"))

        df <- df %>%
          dplyr::select(dplyr::contains("_perday"))

        linregress <- stats::lm(eload_perday ~ ., data = df)

      } else {

        df <- training_data %>%
          dplyr::select(-c("time", "temp", "HDD_perday", "CDD_perday", "days", "CDD"))

        df <- df %>%
          dplyr::select(! dplyr::contains("_perday"))

        linregress <- stats::lm(eload ~ ., data = df)

      }

    } else if (nterval_value == "Daily") {

      df <- training_data %>%
        dplyr::select(-c("time", "temp", "CDD"))

      linregress <- stats::lm(eload ~ ., data = df)

    }


  } else if (model_input_options$regression_type == "CDD Regression" | model_input_options$regression_type == "CDD") {

    if (nterval_value == "Monthly") {

      if (model_input_options$day_normalized == TRUE){

        df <- training_data %>%
          dplyr::select(-c("time", "temp", "HDD", "CDD", "days", "HDD_perday"))

        df <- df %>%
          dplyr::select(dplyr::contains("_perday"))

        linregress <- stats::lm(eload_perday ~ ., data = df)

      } else {

        df <- training_data %>%
          dplyr::select(-c("time", "temp", "HDD_perday", "CDD_perday", "days", "HDD"))

        df <- df %>%
          dplyr::select(! dplyr::contains("_perday"))

        linregress <- stats::lm(eload ~ ., data = df)

      }

    } else if (nterval_value == "Daily") {

      df <- training_data %>%
        dplyr::select(-c("time", "temp", "HDD"))

      linregress <- stats::lm(eload ~ ., data = df)

    }

  }

  out <- list()
  out$model <- linregress
  out$model_stats <- dplyr::bind_cols("Variable" = rownames(summary(linregress)$coeff), as.data.frame(summary(linregress)$coeff))

  # Rename model variables appropriately
  if ("HDD" %in% out$model_stats$Variable) {
    out$model_stats["HDD", "Variable"] <- paste0("HDD_",HDD_temp)
  }

  if ("CDD" %in% out$model_stats$Variable) {
    out$model_stats["CDD", "Variable"] <- paste0("CDD_",CDD_temp)
  }


  if (nterval_value == "Monthly"){
    if (model_input_options$day_normalized == TRUE) {
      out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values * training_data$days)
    } else {
      out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
    }
  } else {
    out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)
  }
  out$model_input_options <- model_input_options




  return(out)
}
