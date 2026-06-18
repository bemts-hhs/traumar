#' @title Check if Elements Are Not in a Vector
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `%not_in%` has been **deprecated** because base R (≥ 4.6.0) now provides the
#' `%notin%` operator.
#'
#' Please use `%notin%` from `base` instead.
#'
#' @param x A vector of values to be checked.
#'
#' @param y A vector of values to check against.
#'
#' @returns A logical vector of the same length as `x`, where `TRUE` indicates
#'   the corresponding element in `x` is not found in `y`, and `FALSE` indicates
#'   it is found in `y`.
#'
#' @examples
#'
#' # Example vectors
#' x <- c("apple", "banana", "cherry")
#' y <- c("banana", "grape")
#'
#' # Check which elements in `x` are not in `y`
#' x %not_in% y
#'
#' # Example with numeric values
#' a <- c(1, 2, 3, 4, 5)
#' b <- c(2, 4, 6)
#'
#' a %not_in% b
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
`%not_in%` <- function(x, y) {
  # this function is now deprecated as of 1.2.7
  lifecycle::deprecate_warn(
    when = "1.2.7",
    what = "`%not_in%`()",
    with = "base::`%notin%`()",
    details = "`base` R includes the %notin% operator as of R >= 4.6.0."
  )
  !(x %in% y)
}
