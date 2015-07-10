#' format_headers()
#'
#' This function conducts format headers so all variable names are lower case, and separated by an underscore
#' @param data dataframe: a dataframe
#' @return dataframe
#' @export
#' @examples
#' my_dataframe <- laycUtils::format_headers(my_dataframe)

format_headers <- function(df) {
  # CHECK: that input is a dataframe
  assertthat::assert_that(is.data.frame(df))
  
  headers <- colnames(df)
  headers <- tolower(headers)
  headers <- gsub("[^[:alnum:]]+", "_", headers)
  headers <- gsub("^_|_$", "", headers)
  colnames(df) <- headers
  
  return(df)
}
