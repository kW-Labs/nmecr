#' Temperature data.
#'
#' A dataset containing the daily average temperature for the time period: 04/07/2017 - 04/07/2019.
#'
#'
#' @format A data frame with 732 rows and 2 variables:
#' \describe{
#'   \item{time}{timestamp, format mm/dd/yy}
#'   \item{temp}{outside air temperature corresponding to the timestamp, in deg F}
#'   ...
#' }
"temp"


#' Energy Use data.
#'
#' A dataset containing the daily average temperature for the time period: 04/07/2017 - 04/07/2019.
#'
#'
#' @format A data frame with 732 rows and 2 variables:
#' \describe{
#'   \item{time}{timestamp, format mm/dd/yy}
#'   \item{eload}{energy consumption data aggregated to the daily interval, in kWh}
#'   ...
#' }
"eload"

#' Temperature data for a school.
#'
#' A dataset containing the hourly average temperature for the time period: 01/01/2018 - 12/31/2018.
#'
#'
#' @format A data frame with 8760 rows and 2 variables:
#' \describe{
#'   \item{time}{timestamp, format mm/dd/yy hh:mm:ss}
#'   \item{temp}{outside air temperature corresponding to the timestamp, in deg F}
#'   ...
#' }
"school_temp"


#' Energy Use data of a school.
#'
#' A dataset containing the hourly average temperature for the time period: 01/01/2018 - 12/31/2018.
#'
#'
#' @format A data frame with 8760 rows and 2 variables:
#' \describe{
#'   \item{time}{timestamp, format mm/dd/yy hh:mm:ss}
#'   \item{eload}{energy consumption data aggregated to the hourly interval, in kWh}
#'   ...
#' }
"school_eload"



#' Operating mode information data of a school.
#'
#' A dataset containing the hourly average temperature for the time period: 01/01/2018 - 12/31/2018.
#'
#'
#' @format A data frame with 8760 rows and 5 variables:
#' \describe{
#'   \item{time}{timestamp, format mm/dd/yy hh:mm:ss}
#'   \item{school_holidays}{indicator variable marking the school holidays in the given time period}
#'   \item{summer maintenance}{indicator variable marking periods of maintenance during the given time period}
#'   \item{summer school}{indicator variable marking the days of summer school in the given time period}
#'   \item{pre-class ramp up}{indicator variable marking days of pre-class activity ramp up before the Fall session}
#'   ...
#' }
"school_op_mode"
