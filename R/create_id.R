#' create_id()
#'
#' Helper function. Create custom id by concatenating different variables
#' @param var character vector: list of variables to be concatenated to form the new id
#' @param df dataframe
#' @return character vector
#' @export
#' @examples
#' create_id(df, c('first_name', 'last_name')

create_id <- function(df, var = c("first_name", "last_name")) {
  # check that var contains valid variable names from df var %in% colnames(df)
  stopifnot(all(var %in% colnames(df)))
  # concatenate create a new column `x` with the three columns collapsed together
  out <- do.call(paste0, df[var])
  # Remove all non alphabetic characters (including spaces)
  out <- gsub("[^[:alpha:]]", "", out)
  # Change to lowercase id_name
  out <- tolower(out)
  
  return(out)
}