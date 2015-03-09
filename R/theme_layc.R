##' Theme for LAYC plots
##'
##' Inspired by theme_few() from ggthemes package
##'
##' @references ggthemes
##' \url{https://github.com/jrnold/ggthemes}.
##'
##' @inheritParams ggplot2::theme_bw
##' @family themes few
##' @export
##' @examples
##' plot <- ggplot(diamonds, aes(x = , y = )) + geom_point()
##' layc_plot <- plot + theme_layc

theme_layc <- function (base_size = 12, base_family = "Calibri") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2))
}