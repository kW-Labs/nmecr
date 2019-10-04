#' Calculate coverage of independent variables in the normalized conditions dataset.
#'
#' \code{This function calculates the coverage of the independent variables in the normalized conditions dataset to
#' inform the user about the data range within which the model predictions are valid.}
#'
#' @param site_temp_data An nx2 dataframe with the temperature data of the facility. Colnames: time and temp.
#' @param ref_temp_data An nx2 dataframe with the hourly normalized temperature data corresponding to the facility's location. Colnames: time and temp.
#' @param outlier_threshold a numeric indicating the number of datapoints (hours or days, depending on the data interval) below which the temperature observation occurences will be considered an outlier.
#' Default values: 1 for daily data, up to 10 for hourly data.
#' @param extrapolation_limit A numeric indicating the percentage, beyond the minimum and maximum observed temperatures, up to which the data range may be extrapolated for model prediction.
#' @param site_temp_start a Date object indicating the baseline start date.
#' @param site_temp_end A Date object indicating the baseline end date.
#' @param data_interval Character string specifying the data time interval: "Hourly", "Daily, or "Monthly".
#'
#' @return a list with the following components:
#' \describe{
#'   \item{temp_coverage}{a data frame with temperature bins and associated frequencies of occurence from site temperature data and normalized temperature data.}
#'   \item{coverage_factor_summary}{a dataframe with calculated temperature and time coverage factors, counts of covered and uncovered datapoints in the normalized conditions dataset.}
#'   \item{temp_bin_summary}{a list with min, max, extrapolated min, and extrapolated max observed temperature values.}
#' }
#' @export



calculate_coverage <- function(site_temp_data, ref_temp_data,
                               outlier_threshold = NULL, extrapolation_limit = NULL,
                               site_temp_start = NULL, site_temp_end = NULL, data_interval = NULL) {

  site_temp_data <- site_temp_data[complete.cases(site_temp_data), ]
  ref_temp_data <- ref_temp_data[complete.cases(ref_temp_data), ]

  site_temp_data_nterval <- difftime(site_temp_data$time[2],
                                     site_temp_data$time[1], units = "min")

  if (site_temp_data_nterval == 15) {
    site_temp_data_interval <- "15-min"
  } else if (site_temp_data_nterval < 60) {
    site_temp_data_interval <- "less than 60-min"
  } else if (site_temp_data_nterval == 60) {
    site_temp_data_interval <- "Hourly"
  } else if (site_temp_data_nterval == 1440) {
    site_temp_data_interval <- "Daily"
  }

  if (site_temp_data_interval == "15-min" | site_temp_data_interval == "less than 60-min"){
    site_temp_data <- agg_to_hourly(site_temp_data, "Mean")
    colnames(site_temp_data) <- c("time", "temp")
  }

  site_temp_data <- site_temp_data %>%
    filter(time > site_temp_start - 1 &
             time < site_temp_end + 1 )

  mround <- function(number, multiple=2) {
    n <- number / multiple
    if (abs(n - trunc(n)) == 0.5) {
      n <- n - 0.1
    } else {
      n <- n
    }
    round(n) * multiple
  }

  if (site_temp_data_interval == "15-min" && data_interval == "Hourly" |
      site_temp_data_interval == "Hourly" && data_interval == "Hourly"){

    ref_temp_data <- ref_temp_data
    site_temp_data <- site_temp_data

  } else if (site_temp_data_interval == "Hourly" && data_interval == "Daily" |
             site_temp_data_interval == "15-min" && data_interval == "Daily") {

    site_temp_data <- agg_to_daily(site_temp_data, "Mean")
    colnames(site_temp_data) <- c("time", "temp")

    ref_temp_data <- agg_to_daily(ref_temp_data, "Mean")
    colnames(ref_temp_data) <- c("time", "temp")

  } else if (site_temp_data_interval == "Daily" && data_interval == "Daily") {

    ref_temp_data <- ref_temp_data
    site_temp_data <- site_temp_data
  }

  # Creating an empty temperature bin matrix
  bins <- seq(0, 120, by = 2)
  bins <- as.factor(bins)
  temp_data <- as.data.frame(bins)
  colnames(temp_data) <- c("bins")

  # Binning site temperature data

  site_temp_bins <- sapply(site_temp_data$temp, mround)
  site_temp_bins <- as.data.frame(site_temp_bins)
  colnames(site_temp_bins) <- c("bins")

  count_site_bins <- as.data.frame(table(site_temp_bins))
  count_site_bins <- as.data.frame(count_site_bins)
  colnames(count_site_bins) <- c("bins", "frequency")

  # Populating the temperature bin matrix created above

  temp_coverage <- left_join(temp_data, count_site_bins, by = "bins")
  colnames(temp_coverage) <- c("bins", "site_data")
  temp_coverage$site_data[is.na(temp_coverage$site_data)] <- 0

  # Binning reference temperature data

  ref_data_bins <- sapply(ref_temp_data[[2]], mround)
  ref_data_bins <- as_data_frame(ref_data_bins)
  colnames(ref_data_bins) <- c("bins")

  count_ref_data_bins <- as.data.frame(table(ref_data_bins))
  colnames(count_ref_data_bins) <- c("bins", "frequency")

  # Populating the temperature bin matrix created above

  temp_coverage <- left_join(temp_coverage, count_ref_data_bins, by = "bins")
  colnames(temp_coverage) <- c("bins", "n_site_data", "n_ref_data")
  temp_coverage$n_ref_data[is.na(temp_coverage$n_ref_data)] <- 0

  temp_coverage$bins <- as.numeric(temp_coverage$bins)

  out <- NULL
  out$temp_coverage <- temp_coverage

  # Calculating Coverage Factor

  outlier_threshold <- outlier_threshold
  max_obs_OA_bin <- max(as.numeric(temp_coverage$bins[temp_coverage$n_site_data > outlier_threshold |
                                                        temp_coverage$n_site_data == outlier_threshold]))
  min_obs_OA_bin <- min(as.numeric(temp_coverage$bins[temp_coverage$n_site_data > outlier_threshold |
                                                        temp_coverage$n_site_data == outlier_threshold]))

  obs_weather_range <- max_obs_OA_bin - min_obs_OA_bin
  extrapolation_limit <- extrapolation_limit

  max_ref_OA_bin <- max(as.numeric(temp_coverage$bins[temp_coverage$n_ref_data > 0.1]))
  min_ref_OA_bin <- min(as.numeric(temp_coverage$bins[temp_coverage$n_ref_data > 0.1]))

  extrapolated_max_obs_OA_bin <- mround(max_obs_OA_bin + (extrapolation_limit * obs_weather_range),2)
  extrapolated_min_obs_OA_bin <- mround(min_obs_OA_bin - (extrapolation_limit * obs_weather_range),2)

  temp_coverage_factor <- signif(min(100, (100 * (extrapolated_max_obs_OA_bin - extrapolated_min_obs_OA_bin) /
                                             (max_ref_OA_bin - min_ref_OA_bin))), 4)

  if (data_interval == "Hourly") {

    peak_hours <- max(temp_coverage$n_site_data)
    peak_bin_hourly <- as.numeric(temp_coverage$bins[which(temp_coverage$n_site_data == max(temp_coverage$n_site_data))])

    hours_covered <- temp_coverage %>%
      filter(bins <= extrapolated_max_obs_OA_bin & bins >= extrapolated_min_obs_OA_bin)

    hours_covered <- sum(hours_covered$n_ref_data)

    hours_not_covered <- 8760 - hours_covered
    hourly_coverage_factor <- signif((hours_covered / 8760) * 100, 4)

    coverage_factor_summary <- as.data.frame(matrix(nr = 6, nc = 2))
    names(coverage_factor_summary) <- c("Coverage Factor Summary", "")

    coverage_factor_summary[1, 1] <- "Temperature Coverage"
    coverage_factor_summary[1, 2] <- paste(temp_coverage_factor, "%", sep = "")

    coverage_factor_summary[2, 1] <- "Time Coverage (Hours)"
    coverage_factor_summary[2, 2] <- paste(hourly_coverage_factor, "%", sep = "")

    coverage_factor_summary[3, 1] <- "Hours Covered"
    coverage_factor_summary[3, 2] <- format(hours_covered, scientific = FALSE, big.mark = ",", digits = 2)

    coverage_factor_summary[4, 1] <- "Hours Not Covered"
    coverage_factor_summary[4, 2] <- format(hours_not_covered, scientific = FALSE, big.mark = ",", digits = 2)

    coverage_factor_summary[5, 1] <- "Peak Temperature Bin"
    coverage_factor_summary[5, 2] <- format(peak_bin_hourly, scientific = FALSE, big.mark = ",", digits = 2)

    coverage_factor_summary[6, 1] <- "Number of hours in Peak Temperature Bin"
    coverage_factor_summary[6, 2] <- format(peak_hours, scientific = FALSE, big.mark = ",", digits = 2)

  } else if (data_interval == "Daily") {

    peak_days <- max(temp_coverage$n_site_data)
    peak_bin_daily <- as.numeric(temp_coverage$bins[which(temp_coverage$n_site_data == max(temp_coverage$n_site_data))])

    days_covered <- temp_coverage %>%
      filter(bins <= extrapolated_max_obs_OA_bin & bins >= extrapolated_min_obs_OA_bin)

    days_covered <- sum(days_covered$n_ref_data)

    days_not_covered <- 365 - days_covered
    daily_coverage_factor <- signif((days_covered / 365) * 100, 4)

    coverage_factor_summary <- as.data.frame(matrix(nr = 6, nc = 2))
    names(coverage_factor_summary) <- c("Coverage Factor Summary", "")

    coverage_factor_summary[1, 1] <- "Temperature Coverage"
    coverage_factor_summary[1, 2] <- paste(temp_coverage_factor, "%", sep = "")

    coverage_factor_summary[2, 1] <- "Time Coverage (Days)"
    coverage_factor_summary[2, 2] <- paste(daily_coverage_factor, "%", sep = "")

    coverage_factor_summary[3, 1] <- "Days Covered"
    coverage_factor_summary[3, 2] <- format(days_covered, scientific = FALSE, big.mark = ",", digits = 2)

    coverage_factor_summary[4, 1] <- "Days Not Covered"
    coverage_factor_summary[4, 2] <- format(days_not_covered, scientific = FALSE, big.mark = ",", digits = 2)

    coverage_factor_summary[5, 1] <- "Peak Temperature Bin"
    coverage_factor_summary[5, 2] <- format(peak_bin_daily, scientific = FALSE, big.mark = ",", digits = 2)

    coverage_factor_summary[6, 1] <- "Number of days in Peak Temperature Bin"
    coverage_factor_summary[6, 2] <- format(peak_days, scientific = FALSE, big.mark = ",", digits = 2)

  }

  out$coverage_factor_summary <- coverage_factor_summary

  temp_bin_summary <- as.data.frame(matrix(nr = 4, nc = 2))
  names(temp_bin_summary) <- c("Temperature Bin Summary", "")

  temp_bin_summary[1, 1] <- "Min Observed Temp Bin"
  temp_bin_summary[1, 2] <- min_obs_OA_bin

  temp_bin_summary[2, 1] <- "Max Observed Temp Bin"
  temp_bin_summary[2, 2] <- max_obs_OA_bin

  temp_bin_summary[3, 1] <- "Extrapolated Min Temp Bin"
  temp_bin_summary[3, 2] <- extrapolated_min_obs_OA_bin

  temp_bin_summary[4, 1] <- "Extrapolated Max Temp Bin"
  temp_bin_summary[4, 2] <- extrapolated_max_obs_OA_bin

  out$temp_bin_summary <- temp_bin_summary

  return(out)
}
