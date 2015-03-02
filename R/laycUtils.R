#' load_csv()
#'
#' Helper function. Set default options for the read.csv function
#' @param file character vector: the name of the file which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
#' @return data frame
#' @keywords internal
#' @export
#' @examples
#' load_csv('./data/my_csv_file.csv')

load_csv <- function(file) {
  out <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, na.strings = "")
  out[] <- lapply(out, tolower)
  
  return(out)
}

#' load_txt()
#'
#' Helper function. Set default options for the read.table function
#' @param file character vector: path .txt file containing data to be loaded. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
#' @return data frame
#' @keywords internal
#' @export
#' @examples
#' load_txt('./data/my_txt_file.txt')
load_txt <- function(file) {
  out <- read.table(file, header = TRUE, sep = '\t', quote = "", comment.char = "", na.strings = "")
  out[] <- lapply(out, tolower)
  
  return(out)
}
