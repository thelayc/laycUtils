#' load_txt()
#'
#' Helper function. Set default options for the read.table function
#' @param file character vector: path .txt file containing data to be loaded. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
#' @param ... Other arguments to be passed to load_txt(). See ?read.table to get the list of possible arguments
#' @return data frame
#' @keywords internal
#' @export
#' @examples
#' load_txt('./data/my_txt_file.txt')
load_txt <- function(file, ...) {
  out <- read.table(file, header = TRUE, sep = '\t', quote = "", comment.char = "", na.strings = "", stringsAsFactors = FALSE, colClasses = "character", ...)
  
  return(out)
}