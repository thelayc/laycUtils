#' plot_change()
#'
#' Function that creates a plot from numeric variable showing change
#' @param var: The variable to plot
#' @return dataframe
#' @export
#' @examples
#' df <- id_prepost(df, date = 'test_date') 

plot_change <- function(df, my_palette = c('#2CA02C', '#FF7F0E', '#D62728')) {
  
  df <- data.frame(table(df$rit_change))
  colnames(df) <- c('change', 'n')
  df$percent <- df$n / sum(df$n)
  
  my_title <- paste0('Percentage of students showing postive / negative change in RIT scores\ncomparison between first and last test taken\nschool year 2014-15\nn = ', sum(df$n))
  p <- ggplot(data = df, aes(x = reorder(change, c(3,2,1)), y = percent))
  p <- p + geom_point(aes(color = change), size = rel(10), alpha = .8)
  p <- p + geom_text(aes(y = percent + 0.06, label = percent(percent)), vjust = -.8)
  p <- p + theme_hc()
  p <- p + scale_y_continuous(limits = c(0, 1))
  p <- p + scale_color_manual(values = my_palette)
  p <- p + ggtitle(my_title)
  p <- p + theme(
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    legend.position = 'none'
  )
  p <- p + coord_flip()
}