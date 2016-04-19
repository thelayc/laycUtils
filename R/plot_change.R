#' plot_change()
#'
#' Takes a continuous variable with values from -Inf to +Inf, recodes the variable into three categories: negative, no change, positive, and plot the results.
#' 
#' @param var: The variable to plot
#' @return dataframe
#' @export


plot_change <- function(var, my_palette = c('#2CA02C', '#FF7F0E', '#D62728')) {
  
  df <- data.frame(table(var))
  colnames(df) <- c('change', 'n')
  df$percent <- df$n / sum(df$n)
  
  p <- ggplot(data = df, aes(x = reorder(change, c(3,2,1)), y = percent))
  p <- p + geom_point(aes(color = change), size = rel(10), alpha = .8)
  p <- p + geom_text(aes(y = percent + 0.06, label = scales::percent(percent)), vjust = -.8)
  p <- p + ggthemes::theme_hc()
  p <- p + scale_y_continuous(limits = c(0, 1))
  p <- p + scale_color_manual(values = my_palette)
  p <- p + theme(
    text = element_text(family = 'Calibri'),
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    legend.position = 'none'
  )
  p <- p + coord_flip()
  p
}