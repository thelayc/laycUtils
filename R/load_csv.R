#' load_csv()
#'
#' Helper function. Set default options for the read.csv function
#' @param file character vector: the name of the file which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
#' @param ... Other arguments to be passed to load_csv(). See ?read.table to get the list of possible arguments
#' @return data frame
#' @keywords internal
#' @export
#' @examples
#' load_csv('./data/my_csv_file.csv')

load_csv <- function(file, ...) {
  out <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, na.strings = "", colClasses = "character", ...)
  
  return(out)
}