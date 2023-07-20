#' Changepoint algorithms using outside air temperature.
#'
#' \code{This function builds an energy use model using the four changepoint algorithms: Three Parameter Cooling (3PC), Three Parameter Heating (3PH),
#' Four Parameter Linear Model (4P), Five Parameter Linear Model (5P).}
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


model_with_CP <- function(training_data = NULL, model_input_options = NULL){

  Estimate <- NULL # No visible binding for global variable

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

  independent_variable <- training_data$temp
  names(training_data)[names(training_data) == "temp"] <- "independent_variable"

  if (nterval_value == "Monthly"){

    if (model_input_options$day_normalized == TRUE) {
      dependent_variable <- training_data$eload_perday
      names(training_data)[names(training_data) == "eload_perday"] <- "dependent_variable"

      keep <- training_data %>%
        dplyr::select(dplyr::contains("_perday")) %>%
        dplyr::select(-c("HDD_perday", "CDD_perday", "days"))

    } else {

      dependent_variable <- training_data$eload
      names(training_data)[names(training_data) == "eload"] <- "dependent_variable"

      keep <- training_data %>%
        dplyr::select(! dplyr::contains("_perday")) %>%
        dplyr::select(-c("HDD", "CDD", "days"))

    }
  }

  if (nterval_value == "Daily") {
    dependent_variable <- training_data$eload
    names(training_data)[names(training_data) == "eload"] <- "dependent_variable"

    keep <- training_data %>%
      dplyr::select(-c("HDD", "CDD"))

  } else if (nterval_value == "Hourly" | nterval_value == "15-min") {
    dependent_variable <- training_data$eload
    names(training_data)[names(training_data) == "eload"] <- "dependent_variable"

    keep <- training_data

  }

  if (model_input_options$regression_type == "Three Parameter Cooling" | model_input_options$regression_type == "3PC") {

    dummy_cooling_model <- stats::lm(dependent_variable  ~ 1) # constrained slope
    three_paramter_cooling_model <- tryCatch(segmented::segmented(dummy_cooling_model, seg.Z = ~independent_variable),
                                             warning = function(w) w)

    if(methods::is(three_paramter_cooling_model, "warning")) {
      df <- 'A 3PC model could not be computed. No breakpoint found'
    } else {

      model_input_options$estimated_breakpoint <- dplyr::bind_cols("Breakpoints" = rownames(segmented::summary.segmented(three_paramter_cooling_model)$psi), as.data.frame(segmented::summary.segmented(three_paramter_cooling_model)$psi))

      out <- list()
      out$model_input_options <- model_input_options

      initial_model <- dplyr::bind_cols("Variable" = rownames(segmented::summary.segmented(three_paramter_cooling_model)$coeff),
                                        as.data.frame(segmented::summary.segmented(three_paramter_cooling_model)$coeff))

      remove <- initial_model %>%
        dplyr::filter(Estimate == 0)

      df <- three_paramter_cooling_model$model[! names(three_paramter_cooling_model$model) %in% remove$Variable]
    }

  } else if (model_input_options$regression_type == "Three Parameter Heating" | model_input_options$regression_type == "3PH") {

    independent_variable <- - independent_variable # flipping the sign of the independent variable for a constrained sloped
    dummy_heating_model <- stats::lm(dependent_variable ~ 1)

    three_paramter_heating_model <- tryCatch(segmented::segmented(dummy_heating_model, seg.Z = ~ independent_variable),
                                             warning = function(w) w)

    if(methods::is(three_paramter_heating_model, "warning")) {
      df <- 'A 3PH model could not be computed. No breakpoint found'
    } else {
      model_input_options$estimated_breakpoint <- dplyr::bind_cols("Breakpoints" = rownames(segmented::summary.segmented(three_paramter_heating_model)$psi), as.data.frame(segmented::summary.segmented(three_paramter_heating_model)$psi))
      model_input_options$estimated_breakpoint[c(2,3)] <- - model_input_options$estimated_breakpoint[c(2,3)] # flip the sign of the breakpoint back

      out <- list()
      out$model_input_options <- model_input_options

      initial_model <- dplyr::bind_cols("Variable" = rownames(segmented::summary.segmented(three_paramter_heating_model)$coeff),
                                        as.data.frame(segmented::summary.segmented(three_paramter_heating_model)$coeff))

      remove <- initial_model %>%
        dplyr::filter(Estimate == 0)

      df <- three_paramter_heating_model$model[! names(three_paramter_heating_model$model) %in% remove$Variable]
    }



  } else if (model_input_options$regression_type == "Four Parameter Linear Model" | model_input_options$regression_type == "4P") {

    linear_4P_model <- stats::lm(dependent_variable ~ independent_variable)
    four_paramter_linear_model <- segmented::segmented(linear_4P_model, seg.Z = ~independent_variable)

    model_input_options$estimated_breakpoint <- dplyr::bind_cols("Breakpoints" = rownames(segmented::summary.segmented(four_paramter_linear_model)$psi), as.data.frame(segmented::summary.segmented(four_paramter_linear_model)$psi))

    out <- list()
    out$model_input_options <- model_input_options

    initial_model <- dplyr::bind_cols("Variable" = rownames(segmented::summary.segmented(four_paramter_linear_model)$coeff),
                                      as.data.frame(segmented::summary.segmented(four_paramter_linear_model)$coeff))

    remove <- initial_model %>%
      dplyr::filter(Estimate == 0)

    df <- four_paramter_linear_model$model[! names(four_paramter_linear_model$model) %in% remove$Variable]

  } else if (model_input_options$regression_type == "Five Parameter Linear Model" | model_input_options$regression_type == "5P") {
    
    # Check if breakpoints are away from min and max of data (10% quantiles)
    init.breakpoint.10percent = quantile(training_data$independent_variable, c(.1, .9), na.rm = TRUE)
    if (model_input_options$initial_breakpoints[1] < as.numeric(init.breakpoint.10percent[1])) {
      model_input_options$initial_breakpoints[1] = as.numeric(init.breakpoint.10percent[1])
    }
    
    if (model_input_options$initial_breakpoints[2] > as.numeric(init.breakpoint.10percent[2])) {
      model_input_options$initial_breakpoints[2] = as.numeric(init.breakpoint.10percent[2])
    }

    linear_5P_model <- stats::lm(dependent_variable ~ independent_variable)

    if.false <- F
#    while (if.false == F){
    tryCatch ({
      five_paramter_linear_model <- segmented::segmented(linear_5P_model, seg.Z = ~independent_variable, psi = model_input_options$initial_breakpoints)
#      if.false <- T
    }, error = function(e){
    }, finally = {})
#    }

    model_input_options$estimated_breakpoint <- dplyr::bind_cols("Breakpoints" = rownames(segmented::summary.segmented(five_paramter_linear_model)$psi), as.data.frame(segmented::summary.segmented(five_paramter_linear_model)$psi))

    out <- list()
    out$model_input_options <- model_input_options

    initial_model <- dplyr::bind_cols("Variable" = rownames(segmented::summary.segmented(five_paramter_linear_model)$coeff),
                                      as.data.frame(segmented::summary.segmented(five_paramter_linear_model)$coeff))

    remove <- initial_model %>%
      dplyr::filter(Estimate == 0)

    df <- five_paramter_linear_model$model[! names(five_paramter_linear_model$model) %in% remove$Variable]

  }

  keep_temp <- keep %>%
    dplyr::select(-c("time", "dependent_variable", "independent_variable"))

  if (class(df) == 'character') {
    message(df)
  } else {

    df <- df %>%
      dplyr::bind_cols(keep_temp)

    if (model_input_options$regression_type == "Three Parameter Heating" | model_input_options$regression_type == "3PH" |
        model_input_options$regression_type == "Three Parameter Cooling" | model_input_options$regression_type == "3PC" ) {

      df <- df %>%
        dplyr::select(-"independent_variable")
    }

    linregress <- stats::lm(dependent_variable ~ ., data = df)

    out$model <- linregress
    out$model_stats <- dplyr::bind_cols("Variable" = rownames(summary(linregress)$coeff), as.data.frame(summary(linregress)$coeff))

    if (nterval_value == "Monthly"){
      if (model_input_options$day_normalized == TRUE) {
        out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values * training_data$days)

        names(out$training_data)[names(out$training_data) == "dependent_variable"] <- "eload_perday"
      } else {
        out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)

        names(out$training_data)[names(out$training_data) == "dependent_variable"] <- "eload"
      }
    } else {
      out$training_data <- data.frame(training_data, "model_fit" = linregress$fitted.values)

      names(out$training_data)[names(out$training_data) == "dependent_variable"] <- "eload"
    }

    names(out$training_data)[names(out$training_data) == "independent_variable"] <- "temp"

    return(out)
  }

}
