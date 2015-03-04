#' check_activity()
#'
#' This function checks if active ETO partipants have some service activity recorded through either "point of service", "touchpoint", "assessment" or "referrals".
#' @param enroll_file character vector: the name of the file which the enrollment data are to be read from. This must be a .txt file exported from the [Admin] raw_enrollment_report
#' @param pos_file character vector: the name of the file which the "point of service" data are to be read from. This must be a .txt file exported from the `[Admin] raw_pos_report`
#' @param tp_file character vector: the name of the file which the "touchpoint" data are to be read from. This must be a .txt file exported from the `[Admin] raw_touchpoint_report`
#' @param asmt_file character vector: the name of the file which the "assessment" data are to be read from. This must be a .txt file exported from the `[Admin] raw_assessment_report`
#' @param ref_file character vector: the name of the file which the "referral" data are to be read from. This must be a .txt file exported from the `[Admin] raw_referral_report
#' @param save_output logical: FALSE: Return output on screen, TRUE: Save output in the "output" folder in the working directory
#' @return plot and data frame
#' @export
#' @examples
#' check_activity(enroll_file = './data/enroll.txt', pos_file = './data/pos.txt', asmt_file = './data/asmt.txt', ref_file = './data/ref.txt')

check_activity <- function(enroll_file, pos_file, tp_file, asmt_file, ref_file, save_output = FALSE) {
  # Load data
  enroll <- laycUtils::load_txt(enroll_file)
  pos <- laycUtils::load_txt(pos_file)
  tp <- laycUtils::load_txt(tp_file)
  asmt <- laycUtils::load_txt(asmt_file)
  ref <- laycUtils::load_txt(ref_file)
  
  ## Clean data
  # pos
  pos <- dplyr::select_(pos, 'subject_id', 'program_id')
  pos <- dplyr::distinct_(pos)
  pos$service <- 1
  
  # tp
  tp <- dplyr::select_(tp, 'subject_id', 'program_id')
  tp <- dplyr::distinct_(tp)
  tp$service <- 1
  
  # asmt
  asmt <- dplyr::select_(asmt, 'subject_id', 'program_id')
  asmt <- dplyr::distinct_(asmt)
  asmt$service <- 1
  
  # ref
  ref <- dplyr::select_(ref, 'subject_id', 'program_id')
  ref <- dplyr::distinct_(ref)
  ref$service <- 1
  
  # Combine data sets
  service <- dplyr::bind_rows(pos, tp, asmt, ref)
  service <- dplyr::distinct_(service)
  
  # merge
  merged <- dplyr::left_join(enroll, service, by = c('subject_id', 'program_id'))
  
  ## Keep only problematic records
  issue <- merged[is.na(merged$service), ]
  
  ## Create a summary table
  issue_summary <- dplyr::group_by_(issue, 'program_name')
  issue_summary <- dplyr::summarise(issue_summary, count = length(subject_id)) # Review dplyr for interactive use
  issue_summary <- dplyr::arrange(issue_summary, desc(count))
  
  ## Create summary chart
  my_title <- 'Number of participants active in a program\nwithout any service activity recorded in ETO\n'
  p <- ggplot(issue_summary, aes(x = reorder(program_name, n), y = n)) + coord_flip()
  p <- p + geom_point()
  p <- p + ggtitle(my_title)
  p <- p + ylim(c(0, 200))
 
  
  ## Save list of participants with issue
  if (save_output == TRUE) {
    dir.create('output')
    dir.create('output/issue_by_program')
    write.csv(issue, file = paste0('./output/full_issue_list.csv'), row.names = FALSE, na ="")
    write.csv(issue_summary, file = paste0('./output/issue_summary.csv'), row.names = FALSE, na ="")
    ggplot2::ggsave(file_name = './output/issue_summary.png', plot = p, width = 15, height = 10, units = 'cm')
    
    programs <- sort(unique(issue$program_name))
    
    for (program in programs) {
      out <- issue[issue$program_name == program, ]
      write.csv(out, file = paste0('./output/issue_by_program/', program, '.csv'), row.names = FALSE, na = "")
    }
  } 
  
  return(list(issue, issue_summary))
}