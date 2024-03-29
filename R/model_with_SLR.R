#' Simple Linear Regression using outside air temperature.
#'
#' \code{This function builds an energy use model using the simple linear regression algorithm.}
#'
#' @param training_data Training dataframe and operating mode dataframe. Output from create_dataframe
#' @param model_input_options List with model inputs specified using assign_model_inputs
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


model_with_SLR <- function(training_data = NULL, model_input_options = NULL){

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

  if (nterval_value == "15-min") {

    df <- training_data %>%
      dplyr::select(-c("time"))

    linregress <- stats::lm(eload ~ ., df)

  }  else if (nterval_value == "Hourly") {

    df <- training_data %>%
      dplyr::select(-c("time"))

    linregress <- stats::lm(eload ~ ., df)

  } else if (nterval_value == "Daily") {

    df <- training_data %>%
      dplyr::select(-c("time", "HDD", "CDD"))

    linregress <- stats::lm(eload ~ ., df)

  } else if (nterval_value == "Monthly") {

    if (model_input_options$day_normalized == FALSE) {

      df <- training_data %>%
        dplyr::select(-c("time", "HDD", "CDD", "HDD_perday", "CDD_perday", "days"))

      df <- df %>%
        dplyr::select(! dplyr::contains("_perday"))

      linregress <- stats::lm(eload ~ ., df)

    } else if (model_input_options$day_normalized == TRUE) {

      df <- training_data %>%
        dplyr::select(-c("time", "HDD", "CDD", "HDD_perday", "CDD_perday", "days"))

      df <- df %>%
        dplyr::select(dplyr::contains("_perday") | dplyr::contains("temp"))

      linregress <- stats::lm(eload_perday ~ ., df)
    }

  }

  out <- list()
  out$model <- linregress
  out$model_stats <- dplyr::bind_cols("Variable" = rownames(summary(linregress)$coeff), as.data.frame(summary(linregress)$coeff))

  if (nterval_value == "Monthly") {
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
