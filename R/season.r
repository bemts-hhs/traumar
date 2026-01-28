#' @title Get Season Based on a Date
#'
#' @description
#'
#' This function determines the season (Winter, Spring, Summer, or Fall) based
#' on an input date.
#'
#' The seasons are assigned based on geographic regions similar to how seasons
#' occur in the United States.
#'
#' The seasons are determined using the month of the year and
#' the traditional meteorological definition of seasons (Winter: December,
#' January, February; Spring: March, April, May; Summer: June, July, August;
#' Fall: September, October, November).
#'
#' @param input_date A Date or POSIXct object representing the date to determine
#'   the season for. The input must be of class `Date` or `POSIXct`.
#'
#' @returns A factor indicating the season corresponding to the input date. The
#'   factor levels are:
#'   \itemize{
#'   \item "Winter" for December, January, and February.
#'   \item "Spring" for March, April, and May.
#'   \item "Summer" for June, July, and August.
#'   \item "Fall" for September, October, and November.
#'   \item "Undetermined" if the input is not a valid Date or POSIXct object or
#'   if the month is missing.
#'}
#'
#' @export
#'
#' @examples
#' # Example usage of the season function
#' season(as.Date("2025-01-15"))
#' season(as.POSIXct("2025-07-01 12:00:00"))
#'
#' @author Nicolas Foss, Ed.D., MS
#'
season <- function(input_date) {
  # Check if the value supplied is in fact Date or POSIXct
  validate_class(
    input = input_date,
    class_type = c("date", "date-time"),
    logic = "or",
    type = "error",
    null_ok = TRUE
  )

  # Create the month boundaries of the season based on
  # https://www.weather.gov/dvn/Climate_Astronomical_Seasons
  winter_months <- c(12, 1, 2)
  spring_months <- c(3, 4, 5)
  summer_months <- c(6, 7, 8)
  fall_months <- c(9, 10, 11)

  # Format the input_date as a month number
  month_num <- suppressWarnings(
    # in case input_date is NA or NULL for any given row, supress warning
    # record will be classified as "Undetermined"
    as.numeric(format(input_date, "%m"))
  )

  # Assign the season based on the month number
  factor_result <- ifelse(
    month_num %in% winter_months,
    "Winter",
    ifelse(
      month_num %in% spring_months,
      "Spring",
      ifelse(
        month_num %in% summer_months,
        "Summer",
        ifelse(
          month_num %in% fall_months,
          "Fall",
          ifelse(
            is.na(month_num),
            "Undetermined",
            "Undetermined"
          )
        )
      )
    )
  )

  # Convert result to a factor
  factor_result <- factor(factor_result)

  # Return result
  return(factor_result)
}
