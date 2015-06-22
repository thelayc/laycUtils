#' fuzzy_join()
#'
#' Merge 2 dataframes with inconsistent ID variables (due to typos, for instance). 
#' @param x,y dataframes to merge
#' @param by a character variable to join by. This variable is generally the output of the create_id() function
#' @return dataframe
#' @export
#' @examples
#' fuzzy_join(x = dataframe1, y = dataframe2, by = 'custom_id')

fuzzy_join <- function(x, y, by) {
  
  # CHECK: That both datasets are tidy (no duplicated rows)
  assertthat::assert_that(class(x) == "data.frame")
  assertthat::assert_that(class(y) == "data.frame")
  assertthat::assert_that(by %in% colnames(x))
  assertthat::assert_that(by %in% colnames(y))
  assertthat::assert_that(nrow(x) == length(unique(x[, by])))
  assertthat::assert_that(nrow(y) == length(unique(y[, by])))
  
  # STEP 1: Identify records that are a perfect match
  matched <- dplyr::inner_join(x = x, y = y, by = by)
  matched$match_status = "perfect match"
  
  
  # STEP 2: Identify records that match partially
  # Keep records that don't match perfectly
  temp <- dplyr::anti_join(x = x, y = y, by = by)
  # Apply fuzzy matching
  temp$partials <- as.character(sapply(temp[, by],
                                       FUN = agrep,
                                       y[, by],
                                       max.distance = 0.1, 
                                       value = T))
  # Use NA to identify unmatched records
  temp$partials[temp$partials == "character(0)"] <- NA
  # Bring the original records into the partial match list
  partial_match <- dplyr::inner_join(x = temp, y = y, 
                                     by = c("partials" = by))
  partial_match$match_status = "partial match"
  partial_match["partials"] <- NULL
  
  # STEP 3: Identify unmatched records
  unmatched <- temp[is.na(temp$partials), ]
  unmatched$match_status <- 'unmatched'
  unmatched["partials"] <- NULL
  colnames(unmatched)[colnames(unmatched) == 'raw_id'] <- 'raw_id.x'
  
  # STEP 4: Combine the 3 datasets into one
  out <- dplyr::bind_rows(matched,partial_match, unmatched)
  
  return(out)
}


