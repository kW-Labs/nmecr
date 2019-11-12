#' Generate an energy data model using simple linear regression with outside air temperature.
#'
#' \code{This function builds an energy use model using the simple linear regression algorithm.}
#'
#' @param training_data Training period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param prediction_data Prediction period dataframe, where the columns correspond to the time steps (time), the energy load (eload), and the temperature (Temp).
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#' @param has_operating_modes Boolean specifying whether the energy use profile has varying operating modes. Only used with 'Simple Linear Regression with OAT'.
#' @param train_operating_mode_data dataframe with indicator variables for the various operating modes present in the model training period.
#' Only used with 'Simple Linear Regression with OAT'.
#' @param pred_operating_mode_data dataframe with indicator variables for the various operating modes present in the mdoel prediction period.
#' Only used with 'Simple Linear Regression with OAT'.
#' @param data_units energy data's units
#'
#'
#' @return a list with the following components:
#' \describe{
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{training_data}{a dataframe corresponding to the training data after the
#'   cleaning and filtering function were applied, fitted values, and residuls.}
#'   \item{SLR_model}{an object with parameter coefficients and associated p-values resulting from the SLR model.}
#'   \item{normality metrics}{a list with details on residuals' skewness and kurtosis.}
#'   \item{energy use summary}{Summed baseline, post-implementation, and adjusted baseline energy use values. Assumes training dataset is the
#'   energy project's baseline energy dataset.}
#'   \item{model}{the lm object created within 'model_with_SLR'.}
#'   \item{post_implementation_data}{a dataframe corresponding to the post-implementation dataset along with predicted values.}
#' }
#' @export


model_with_SLR <- function(training_list = NULL, prediction_list = NULL, model_input_options = NULL){

  dframe <- training_list$dataframe

  if(! is.null(training_list$operating_mode_data)){
    dframe <- dplyr::inner_join(dframe, training_list$operating_mode_data, by = "time")
  }

  dframe <- dframe %>%
    dplyr::select(-"time")

  linregress <- lm(eload ~ ., dframe)

  out <- list()
  out$model <- linregress
  out$training_data <- data.frame(training_list$dataframe, "model_fit" = linregress$fitted.values)

  return(out)
}




