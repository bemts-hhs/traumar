#' Classify Dates as Weekday or Weekend
#'
#' This function classifies each date in a vector of dates as either "Weekday"
#' or "Weekend". The function returns "Weekday" for Monday to Friday and
#' "Weekend" for Saturday and Sunday.
#'
#' @param input_date A vector of `Date` or `POSIXct` objects to classify.
#'
#' @return A character vector with the classification for each date: either
#'   "Weekday" or "Weekend".
#'
#' @details The function checks if the `input_date` is a valid `Date` or
#'   `POSIXct` object. It returns "Weekday" for dates that fall on Monday
#'   through Friday and "Weekend" for dates that fall on Saturday or Sunday. If
#'   the input is not of the correct class, the function will throw an error.
#'
#' @examples
#' # Example 1: Date of a weekend
#' weekend(as.Date("2025-01-18"))
#'
#' # Example 2: Date of a weekday
#' weekend(as.Date("2025-01-15"))
#'
#' # Example 3: Date of an invalid object
#' \dontrun{
#' weekend("2025-01-18") # This will throw an error
#' }
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS
#'
weekend <- function(input_date) {

  #check if the value supplied is in fact Date or POSIXct
  if (!lubridate::is.Date(input_date) & !lubridate::is.POSIXct(input_date)) {

    cli::cli_abort(
      paste0(
        "The input to {.var input_date} must be an object of class {.cls Date} or {.cls POSIXct}, but you supplied an object of class {.cls {class(input_date)}}.",
        "i" = "Supply a {.cls Date} object to {.fn weekend}."
      )
    )

  }

  # Get the day of the week
  day_of_week <- base::weekdays(input_date, abbreviate = F)

  # Classify as Weekday or Weekend
  classification <- ifelse(
    day_of_week %in% c("Saturday", "Sunday"), "Weekend",
    base::ifelse(day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Undetermined")
  )

  return(classification)

}
