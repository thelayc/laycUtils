#' clean_data()
#'
#' This function conducts basic data cleaning tasks on a data frame.
#' @param data dataframe: a dataframe containing.
#' @param to_date character vector: Names of columns to be converted to date format.
#' @param to_numeric character vector: Names of columns to be converted to numeric format.
#' @return dataframe
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' clean_data(data = enroll)

clean_data <- function(data,
                     to_date = c('start', 'end', 'date'),
                     to_numeric = c('days_enrolled', 'time')) {
  # Turn everything to lower case to make data manipulation easier
  message('Passing all variables to lower case...')
  data[] <- lapply(data, tolower)
  # Turn date columns to date format
  to_date <- intersect(to_date, colnames(data))
  message(paste('Formatting these variables as date:', 
                paste(to_date, collapse = ", ")))
  data[to_date] <- lapply(data[to_date], lubridate::mdy)
  # Turn numeric columns to numeric
  to_numeric <- intersect(to_numeric, colnames(data))
  num_columns <- stringr::str_detect(colnames(data), 'id$')
  num_columns <- colnames(data)[num_columns]
  num_columns <- c(num_columns, to_numeric)
  message(paste('Formatting these variables as numeric:', 
                paste(num_columns, collapse = ", ")))
  data[num_columns] <- lapply(data[num_columns], tidyr::extract_numeric)
  
  return(data)
}  
  
 