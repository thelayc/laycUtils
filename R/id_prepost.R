#' id_prepost()
#'
#' This is a helper function that identifies 'pre' and 'post' test based on the date the test was taken.
#' @param df dataframe: a dataframe containing longitudinal data.
#' @return dataframe
#' @export
#' @examples
#' df <- id_prepost(df, date = 'test_date') 

id_prepost <- function(df, date) {
  
#   df <- dplyr::group_by_(df, .dots = group_var)
#   df <- dplyr::mutate_(df,
#                        first = lazyeval::interp(~min(v), v = as.name(date)),
#                        last = lazyeval::interp(~max(v), v = as.name(date)))
#   df <- dplyr::ungroup(df)
#   df <- as.data.frame(df)
  
  df$first <- min(df$date)
  df$last <- max(df$date)
  
  
  # Assign pre / post values to first / last date taken
  
  df$prepost[df$date == df$first] <- 'pre'
  df$prepost[df$date == df$last & df$first != df$last] <- 'post'
  df$prepost <- factor(df$prepost, levels = c('pre', 'post'))
  
  # Return dataframe
  return(df)
}