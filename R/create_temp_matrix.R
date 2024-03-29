#' Temperature matrix for model_with_TOWT.
#'
#' \code{This function generates temperature changepoints for subsequent use in energy data fitting}
#'
#' @param temp_col A vector containing temperature data
#' @param temp_knots A vector specifying temperature knots to create temperature data segments
#'
#' @return A matrix with component temperatures

create_temp_matrix <- function(temp_col = NULL, temp_knots = NULL) {
  temp_rows <- length(temp_col)
  temp_knot_rows <- length(temp_knots) + 1
  # lower than lowest; in between; and higher than highest
  temp_knots <- c(- 1000000, temp_knots, 1000000)
  # Adding a knot to make the loop below work out right

  temp_matrix <- matrix(0, nrow = temp_rows, ncol = temp_knot_rows)
  for (row_index in 1 : temp_knot_rows) {
    ok <- (temp_col > temp_knots[row_index]) &
      (temp_col <= temp_knots[row_index + 1])
    ok[is.na(ok)] <- FALSE

    if (row_index == 1) {
      temp_matrix[ok, row_index] <- temp_col[ok]
      temp_matrix[!ok, row_index] <- temp_knots[row_index + 1]
    } else {
      temp_matrix[ok, row_index] <- temp_col[ok] - temp_knots[row_index]
      temp_matrix[temp_col > temp_knots[row_index + 1], row_index] <-
        temp_knots[row_index + 1] - temp_knots[row_index]
    }
  }

  return(temp_matrix)
}
