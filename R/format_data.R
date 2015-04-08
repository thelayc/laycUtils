#' format_data()
#'
#' This function conducts basic data cleaning tasks on a data frame.
#' @param data dataframe: a dataframe containing.
#' @param to_date character vector: Names of columns to be converted to date format.
#' @param to_numeric character vector: Names of columns to be converted to numeric format.
#' @return dataframe
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' format_data(data = enroll)

format_data <- function(data,
                     to_date = c('start$|^start|end$|^end|date'),
                     to_numeric = c('id$|days|time|weight|wage')) {
  # Turn everything to lower case to make data manipulation easier
  message('Passing all variables to lower case...')
  data[] <- lapply(data, tolower)
  # Turn date columns to date format
  to_date <- colnames(data)[grepl(to_date, colnames(data))]
  message(paste('Formatting these variables as date:\n', 
                paste(to_date, collapse = "\n")))
  data[to_date] <- lapply(data[to_date], lubridate::mdy)
  # Turn numeric columns to numeric
  to_numeric <- colnames(data)[grepl(to_numeric, colnames(data))]
  message(paste('Formatting these variables as numeric:\n', 
                paste(to_numeric, collapse = "\n")))
  data[to_numeric] <- lapply(data[to_numeric], tidyr::extract_numeric)
  
  return(data)
}  
  
 
