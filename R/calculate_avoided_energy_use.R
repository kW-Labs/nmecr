#' Calculate energy use avoided in the post-implementation period.
#'
#' \code{This function computes the estimated avoided energy use using the predicted data of the post-implementation period.}
#'
#' @param modeled_data_obj  An object with modeled data results. For example, the output from 'model_with_TOWT' function.
#'
#' @return a list with the following components:
#' \describe{
#'   \item{savings_summary}{a dataframe with absolute savings value and the savings pct value.}
#'   \item{savings_df}{a dataframe with the savings achieved per time interval.}
#'   \item{pct_savings}{a numeric indicating the percent savings achieved.}
#' }
#' @export

calculate_avoided_energy_use <- function(modeled_data_obj){
  pred <- modeled_data_obj$post_implementation_data
  # actual eload during prediction period = post-measure period
  act_post <- pred$eload
  # prediction of eload during prediction period = post period
  pred_post <- pred$pred_eload
  sum_pred_post <- sum(pred_post, na.rm = TRUE)

  # savings:
  savings <- sum((pred_post - act_post), na.rm = TRUE)
  savings_df <- as.data.frame(pred_post - act_post)
  names(savings_df) <- "savings"
  savings_df <- dplyr::bind_cols("time" = pred$time, savings_df)
  pct_savings <- (savings / sum_pred_post)
  baseline_energy_use <- sum(modeled_data_obj$training_data$eload, na.rm = T)

  results <- NULL
  savings_summary <- as.data.frame(matrix(nr = 1, nc = 2))
  names(savings_summary) <- c("Savings", "Savings (%)")

  savings_summary$'Savings' <- formatC(savings, format = "d")
  savings_summary$'Savings (%)' <- paste(round(pct_savings * 100, 2), "%", sep = "")

  results$savings_summary <-  savings_summary
  results$'savings_df' <- savings_df
  results$pct_savings <- round(pct_savings * 100, 2)
  return(results)
}
