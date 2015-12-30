#' categorize()
#'
#' This is a helper function that recode a numeric vector into categories. by default those categories are: positive, no change, negative
#' 
#' @param vector numeric vector to be recoded
#' @param my_breaks either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#' @param my_labels labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation. If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param ... further arguments passed to or from other methods. See \code{\link[base]{cut}} for more information about additional arguments
#' 
#' @return factor 
#' 
#' @export



categorize <- function(vector, 
                       my_breaks = c(-Inf, 0, 1, Inf),
                       my_labels = c('negative', 'no change', 'positive'),
                       right = TRUE,
                       ...) {
  
  out <- cut(vector, breaks = my_breaks, labels = my_labels, right = right, ...)
  
  return(out)
}