#' check_variable
#' Check whether a specific categorical variable only contains expected values
#'
#' @param df dataframe:
#' @param var character: Name of the variable to be checked
#' @param expect character vector: Vector of expected values for \code{var}
#'
#' @return dataframe
#' @export
#'
#' @examples
#' check_variable(df = my_df, var = 'my_variable', expect = c('value1', 'value2'))

check_variable <- function(df, var, expect) {
  
  check <- !(df[, var] %in% expect)
  unexpected <- df[check, ]
  
  if (nrow(unexpected) > 0) {
    message(paste('The', var, 'variable has unexpected values:\n'))
    unexpected
  } else {
    message('All good!!')
  }
}