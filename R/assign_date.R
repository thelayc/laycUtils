#' assign_date()
#'
#' Helper function. Assign date to missing records
#' @param dates character vector: vector of date characters
#' @param new_date character vector: Represent the date to be assigned to missing values
#' @return vector
#' @keywords internal
#' @export
#' @examples
#' assign_date(my_dates, new_date = '01/01/2015')
assign_date <- function(dates, new_date) {
  dates[is.na(dates)] <- new_date
}
