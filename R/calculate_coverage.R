#' Calculate coverage of independent variables in the normalized conditions dataset.
#'
#' \code{This function calculates the coverage of the independent variables in the normalized conditions dataset to
#' inform the user about the data range within which the model predictions are valid.}
#'
#' @param dataframe Training or Prediction dataframe from create_dataframe
#' @param ref_temp_data An nx2 dataframe with the hourly normalized temperature data corresponding to the facility's location. Colnames: time and temp.
#' @param outlier_threshold a numeric indicating the number of datapoints (hours or days, depending on the data interval) below which the temperature observation occurences will be considered an outlier.
#' Default values: 1 for daily data, up to 10 for hourly data.
#' @param extrapolation_limit A numeric, beyond the minimum and maximum observed temperatures, up to which the data range may be extrapolated for model prediction. Default: 0.05
#'
#' @return a list with the following components:
#' \describe{
#'   \item{temp_coverage}{a data frame with temperature bins and associated frequencies of occurence from site temperature data and normalized temperature data.}
#'   \item{coverage_factor_summary}{a dataframe with calculated temperature and time coverage factors, counts of covered and uncovered datapoints in the normalized conditions dataset.}
#'   \item{temp_bin_summary}{a list with min, max, extrapolated min, and extrapolated max observed temperature values.}
#' }
#' @export



calculate_coverage <- function(dataframe = NULL, ref_temp_data = NULL,
                               outlier_threshold = NULL, extrapolation_limit = NULL) {

  nterval <- difftime(dataframe$time[2], dataframe$time[1], units = "min")

  if (nterval == 60){
    dataframe_interval <- "Hourly"
  } else if (nterval == 1440) {
    dataframe_interval <- "Daily"
  } else if (nterval >= 40320) {
    dataframe_interval <- "Monthly"
  }

  if(dataframe_interval == "Monthly") {
    stop("Please upload a dataframe in hourly or daily time data intervals")
  }

  if(! assertive::is_numeric(outlier_threshold)){
    stop("outlier_threshold must be a numeric input")
  }

  if(extrapolation_limit < 0 | extrapolation_limit > 1 | ! assertive::is_numeric(extrapolation_limit)){
    stop("extrapolation_limit must be a numeric input between 0 and 1. Default: 0.05")
  }

  # ref_temp_data time interval
  nterval_ref_temp <- difftime(ref_temp_data$time[2], ref_temp_data$time[1], units = "mins")

  if (nterval_ref_temp == 60) {
    ref_temp_interval <- "Hourly"
  } else if (nterval_ref_temp == 1440) {
    ref_temp_interval <- "Daily"
  } else {
    stop("Please upload ref_temp_data in hourly or daily time data intervals")
  }

  site_temp_data <- dataframe[, c("time", "temp")]

  if(ref_temp_interval == "Daily" | dataframe_interval == "Daily") {

    ref_temp_data <- ref_temp_data %>%
      dplyr::mutate(day = lubridate::floor_date(ref_temp_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()

    site_temp_data <- site_temp_data %>%
      dplyr::mutate(day = lubridate::floor_date(site_temp_data$time, "day")) %>%
      dplyr::group_by("time" = day) %>%
      dplyr::summarize("temp" = mean(temp, na.rm = T)) %>%
      na.omit()
  }

  # bin size: 2
  mround <- function(number, multiple=2) {
    n <- number / multiple
    if (abs(n - trunc(n)) == 0.5) {
      n <- n + 0.1
    } else {
      n <- n
    }
    round(n) * multiple
  }

  # Creating an empty temperature bin matrix
  bins <- seq(0, 120, by = 2)
  temp_data <- as.data.frame(bins)
  temp_data$bins <- as.character(temp_data$bins)
  colnames(temp_data) <- c("bins")

  # Binning site temperature data

  site_temp_data <- site_temp_data %>%
    dplyr::mutate("bins" = sapply(temp, mround))

  site_temp_bins <- site_temp_data$bins
  count_site_bins <- as.data.frame(table(site_temp_bins))
  colnames(count_site_bins) <- c("bins", "frequency")
  count_site_bins$bins <- as.character(count_site_bins$bins)

  # Binning reference temperature data

  ref_temp_data <- ref_temp_data %>%
    dplyr::mutate("bins" = sapply(temp, mround))

  ref_data_bins <- ref_temp_data$bins
  count_ref_data_bins <- as.data.frame(table(ref_data_bins))
  colnames(count_ref_data_bins) <- c("bins", "frequency")
  count_ref_data_bins$bins <- as.character(count_ref_data_bins$bins)

  # Populating the temperature bin

  temp_coverage <- dplyr::left_join(temp_data, count_site_bins, by = "bins")
  colnames(temp_coverage) <- c("bins", "site_data")
  temp_coverage$site_data[is.na(temp_coverage$site_data)] <- 0

  temp_coverage <- dplyr::left_join(temp_coverage, count_ref_data_bins, by = "bins")
  colnames(temp_coverage) <- c("bins", "n_site_data", "n_ref_data")
  temp_coverage$n_ref_data[is.na(temp_coverage$n_ref_data)] <- 0

  out <- NULL
  out$temp_coverage <- temp_coverage

  # Calculating Coverage Factor

  max_obs_OA_bin <- max(as.numeric(temp_coverage$bins[temp_coverage$n_site_data >= outlier_threshold]))
  min_obs_OA_bin <- min(as.numeric(temp_coverage$bins[temp_coverage$n_site_data > outlier_threshold]))

  max_ref_OA_bin <- max(as.numeric(temp_coverage$bins[temp_coverage$n_ref_data > 0.1]))
  min_ref_OA_bin <- min(as.numeric(temp_coverage$bins[temp_coverage$n_ref_data > 0.1]))

  obs_weather_range <- max_obs_OA_bin - min_obs_OA_bin

  extrapolated_max_obs_OA_bin <- mround(max_obs_OA_bin + (extrapolation_limit * obs_weather_range),2)
  extrapolated_min_obs_OA_bin <- mround(min_obs_OA_bin - (extrapolation_limit * obs_weather_range),2)

  temp_coverage_factor <- signif(min(100, (100 * (extrapolated_max_obs_OA_bin - extrapolated_min_obs_OA_bin) /
                                             (max_ref_OA_bin - min_ref_OA_bin))), 4)

  if (dataframe_interval == "Daily") {

    peak_days <- max(temp_coverage$n_site_data)
    peak_bin_daily <- as.numeric(temp_coverage$bins[which(temp_coverage$n_site_data == max(temp_coverage$n_site_data))])

    days_covered <- temp_coverage %>%
      dplyr::filter(bins <= extrapolated_max_obs_OA_bin & bins >= extrapolated_min_obs_OA_bin)

    days_covered <- sum(days_covered$n_ref_data)

    days_not_covered <- 365 - days_covered
    daily_coverage_factor <- signif((days_covered / 365) * 100, 4)

  } else {

    peak_hours <- max(temp_coverage$n_site_data)
    peak_bin_hourly <- as.numeric(temp_coverage$bins[which(temp_coverage$n_site_data == max(temp_coverage$n_site_data))])

    hours_covered <- temp_coverage %>%
      dplyr::filter(bins <= extrapolated_max_obs_OA_bin & bins >= extrapolated_min_obs_OA_bin)

    hours_covered <- sum(hours_covered$n_ref_data)

    hours_not_covered <- 8760 - hours_covered
    hourly_coverage_factor <- signif((hours_covered / 8760) * 100, 4)

  }

  coverage_factor_summary <- as.data.frame(matrix(nr = 6, nc = 2))

  coverage_factor_summary[1, 1] <- "Temperature Coverage"
  coverage_factor_summary[1, 2] <- temp_coverage_factor


  if (dataframe_interval == "Daily") {

    coverage_factor_summary[2, 1] <- "Time Coverage"
    coverage_factor_summary[2, 2] <- daily_coverage_factor

    coverage_factor_summary[3, 1] <- "Days Covered"
    coverage_factor_summary[3, 2] <- days_covered

    coverage_factor_summary[4, 1] <- "Days Not Covered"
    coverage_factor_summary[4, 2] <- days_not_covered

    coverage_factor_summary[5, 1] <- "Peak Temperature Bin"
    coverage_factor_summary[5, 2] <- peak_bin_daily

    coverage_factor_summary[6, 1] <- "Number of days in Peak Temperature Bin"
    coverage_factor_summary[6, 2] <- peak_days

  } else {

    coverage_factor_summary[2, 1] <- "Time Coverage"
    coverage_factor_summary[2, 2] <- hourly_coverage_factor

    coverage_factor_summary[3, 1] <- "Hours Covered"
    coverage_factor_summary[3, 2] <- hours_covered

    coverage_factor_summary[4, 1] <- "Hours Not Covered"
    coverage_factor_summary[4, 2] <- hours_not_covered

    coverage_factor_summary[5, 1] <- "Peak Temperature Bin"
    coverage_factor_summary[5, 2] <- peak_bin_hourly

    coverage_factor_summary[6, 1] <- "Number of hours in Peak Temperature Bin"
    coverage_factor_summary[6, 2] <- peak_hours

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
